# SPEC

Parse notes from files.
Example of a note
```haskell
-- # Note [Title of Note]
-- Hello, this is a note

data ThisIsSourceCode = ThisIsSourceCode

sourceCode :: Text -> m ()
sourceCode = ...
```

The title and the body of the note should be parsed then tagged with a uuid. After 
running the program the note will be re-rendered back to the same file like so
```haskell
-- # Note [Title of Note]
-- Hello, this is a note
-- id:2da8f1f1-c743-4f8f-ad59-ca4706924e9b

data ThisIsSourceCode = ThisIsSourceCode

sourceCode :: Text -> m ()
sourceCode = ...
```
At the same time, the title and body of the note will be persisted in the db. The
program will preserve non note comment and source code, including white space.

