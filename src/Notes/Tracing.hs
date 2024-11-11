{-# LANGUAGE OverloadedRecordDot #-}

module Notes.Tracing
  ( TraceSampling (..),
    MonadTracer,
    withRootTracer,
    nullTracer,
    module Reexports,
  )
where

import OpenTracing.Reporting.Pure (noReporter)
import System.IO.Unsafe (unsafePerformIO)
import Conduit (MonadUnliftIO)
import Control.Exception (bracket)
import Control.Monad.Catch (MonadMask)
import Lens.Micro.Platform ((.~), (^.))
import OpenTracing as Reexports
  ( ActiveSpan,
    FinishedSpan,
    HasTracer (..),
    LogField (..),
    TagVal (..),
    Tracer (..),
    addLogRecord,
    addTag,
    childOf,
    constSampler,
    ctxTraceID,
    probSampler,
    runSampler,
    runTracer,
    spanContext,
    spanOperation,
    spanOpts,
    traced_,
  )
import OpenTracing qualified
import OpenTracing.Reporting.Batch
  ( AtCapacity (..),
    batchOptions,
    batchReporter,
    boptAtCapacity,
    boptQueueSize,
    closeBatchEnv,
    newBatchEnv,
  )
import OpenTracing.Reporting.Stdio (stderrReporter)
import OpenTracing.Standard (newStdEnv, stdTracer)

type MonadTracer r m = (OpenTracing.MonadTracer r m, MonadMask m, MonadUnliftIO m)

data TraceSampling = TraceSampling
  { stdioSamplingRate :: Maybe Double,
    samplingRate :: Maybe Double,
    enableStdioReporter :: Bool
  }

withRootTracer ::
  TraceSampling ->
  (Tracer -> IO a) ->
  IO a
withRootTracer tracingSampling action = do
  let sampler =
        case tracingSampling.samplingRate of
          Just rate
            | rate == 1.0 -> constSampler True
            | rate == 0 -> constSampler False
            | otherwise -> probSampler rate
          _ -> constSampler True

  let withStdioReporter :: ((FinishedSpan -> IO ()) -> IO a) -> IO a
      withStdioReporter action
        | tracingSampling.enableStdioReporter = do
            let batchOpts =
                  batchOptions (traverse_ stderrReporter)
                    & boptQueueSize
                    .~ 1000
                    & boptAtCapacity
                    .~ Drop
            bracket (newBatchEnv batchOpts) closeBatchEnv $ \batchEnv ->
              action (batchReporter batchEnv)
        | otherwise = action (\_ -> pure ())

  withStdioReporter $ \stdioReporter -> do
    env <- newStdEnv (constSampler True)
    let tracer =
          Tracer
            { tracerStart = stdTracer env,
              tracerReport = \span -> liftIO $ do
                let ctx = span ^. spanContext
                case tracingSampling.stdioSamplingRate of
                  Nothing -> stdioReporter span
                  Just stdioSamplingRate ->
                    whenM
                      ( runSampler
                          (probSampler stdioSamplingRate)
                          (ctxTraceID ctx)
                          (span ^. spanOperation)
                      )
                      $ stdioReporter span
                sampled <- runSampler sampler (ctxTraceID ctx) (span ^. spanOperation)
                when sampled $
                  pure ()
            }
    action tracer

nullTracer :: Tracer
nullTracer = 
  Tracer 
    { tracerStart = stdTracer (unsafePerformIO $ newStdEnv $ constSampler True),
      tracerReport = noReporter
    }
    

