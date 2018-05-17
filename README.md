
# Reagan

## Configuration

You can configure the testing protocol by editing the values in the configuration file found in _app/Main.hs_.

Other than the compiler definitions you need to specify the global timeouts for csmith file generation (variable `generatorTimeout`) and for the execution of the generated program (variable `executionTimeout`). 

**All timeouts are specified in seconds**.

### Compiler Definitions

Each test will compile and run a program generated with csmith. On each run some data is collected and stored in the filesystem. For each compiler used you need to specify a compiler defintion. There is a function that should help with that called _mkCompilerDefinition_. The function has the following signature:

```haskell
mkCompilerDefinition :: String       -- ^ Compiler tag
                     -> FilePath     -- ^ Compiler command
                     -> [ByteString] -- ^ Version fetching arguments
                     -> [ByteString] -- ^ Compilation arguments
                     -> Int          -- ^ Compilation Timeout
                     -> CompilerDefinition
```

Each compiler definition has a `tag` which is used when generating files to specify to which compiler the output refers to.

In order to know precisely which version of the compiler has been used we also include the output from the `--version` argument for all compilers (each may have a specific version argument, so we need to have the command line for each).

The compilation arguments represent the arguments that the compiler needs to receive to produce an executable given a source code file. Since the executable and source file are files generated using the temporary files mechanism we use a very simple templating system. We simply replace `##PROGRAM##` with the generated c source file and `##EXECUTABLE##` with the generated executable name.

Compilation Timeout specifies the time to wait for the compiler to finish compilation, in seconds. Default compiler definitions have been generated for the `clang`, `gcc`, `kcc` and `ccomp` compilers.

## Collected data

### Code generation

The following data is collected from code generation:

```haskell
data GeneratedProgram =
  GeneratedProgram { gpTimeout     :: Int               -- ^ Timeout used for generation (seconds)
                   , gpSeed        :: Maybe Integer     -- ^ Generator seed
                   , gpArguments   :: [ByteString]      -- ^ Generator arguments
                   , gpVersion     :: ByteString        -- ^ Version Information
                   , gpOptions     :: Maybe ByteString  -- ^ Generator options (parsed from output)
                   , gpProgramPath :: FilePath          -- ^ Path to generator program
                   , gpRunningTime :: NominalDiffTime   -- ^ Running time
                   } deriving (Show, Eq)
```

### Compilation

The following data is collected from compilation:

```haskell
data CompiledProgram =
   CompiledProgram { cpTimeout        :: Int              -- ^ Timeout used for compilation
                   , cpVersion        :: ByteString       -- ^ Version information
                   , cpOutput         :: ByteString       -- ^ Compiler output
                   , cpError          :: ByteString       -- ^ Compiler errors
                   , cpRunningTime    :: NominalDiffTime  -- ^ Running time
                   , cpExecutablePath :: Maybe FilePath   -- ^ Compiler path
                   , cpCompilerTag    :: String           -- ^ Compiler tag
                   } deriving (Show, Eq)
```

### Execution

The following data is collected from execution:

```haskell
data ExecutionWithChecksum =
  ExecutionWithChecksum { ewcTimeout        :: Int             -- ^ Timeout used for execution
                        , ewcOutput         :: ByteString      -- ^ Execution output
                        , ewcError          :: ByteString      -- ^ Execution errors
                        , ewcChecksum       :: Maybe Int       -- ^ Execution checksum
                        , ewcRunningTime    :: NominalDiffTime -- ^ Running time
                        , ewcExecutablePath :: FilePath        -- ^ Path to executable
                    } deriving (Show, Eq)
```

## Storage format

The storage format is a dump of the previously defined haskell data structures, plus a divergence report. The divergence report is used to focus in on the specific interest of this project, which is to find out the instances where `kcc` diverges from `ccomp`. The divergence report has the following structure:

```haskell
data DivergenceReport =
  DivergenceReport { drFailedCompilation :: [String]        -- ^ Compiler tags that failed compilation.
                   , drWrongChecksum     :: [(String, Int)] -- ^ Compiler tags and divergent checksums.
                   } deriving (Show, Eq)
```

The following files are produced on a successful run (they are placed in a directory named `csmith_seed_<seed>`):

1. _generator.out_ - The generator data structure.
2. _program.c_ - The generated program.
3. &lt;_compiler-tag_&gt;_compiler.out - The compiler data structure.
4. &lt;_compiler-tag_&gt;_execution.out - The execution data structure.
5. _divergence.info_ - The divergence report.

### Useful queries

This query will find all instances where there is a divergent checksum. This is basically what happens if the compiled programs execute the code in an incorrect fashion, and can point out an incorrect compiler implementation.

```shell
$ grep -R -v --include divergence.info 'drWrongChecksum = \[\]' *
```

This query will find instances where compilation failed for some reason. These can point to corner cases in the compiler code where the compilation process either failed with an error, or it took more then the specified time limit. 

Note that this will exclude all instances where the only compiler that failed is CompCert, since it does not support the full C specification.

```shell
$ grep -R -v --include divergence.info 'drFailedCompilation = \["ccomp_default"\]' *
```

Another useful information is to find all cases where the KCC compiler did not detect any errors while compiling and executing.

```shell
## Transform \n into new line and remove the safe_math errors
$ ls | parallel -j+0 --progress "cat {}/kcc_default_*.out | perl -pe 's/\\\\n/\\n/g' | grep -v safe_math > {}/kcc.out"

## Find all programs which don't have any other errors
$ ls | parallel -j+0 --progress "grep -R -L --include kcc.out 'error' {}" > correct.out

## Move them into the `correct` directory
$ mkdir correct && cat correct.out | sed 's@/.*$@@g' | parallel -j+0 --progress "mv -v {} correct"
```

## Design decisions

### Specific compiler options

If you need to test the same compiler, but with different arguments (for example different optimisation options), add a second definition with a new compiler tag (for example `clang_O3`) and with the arguments that you need.

### Supported platforms

The decision was made to only support the ubuntu 14.04 platform, due to `kcc`'s native support for it. This comes with a bit of a price, since the compilers that come with this version of ubuntu are pretty old.

The Docker image will install CompCert and csmith from sources, so they will have the latest version available, but clang and gcc will be a fairly old version.

### Timeout termination

`kcc` will spawn several process during compilation and we need to kill the process tree originating in the initial compilation process. However since this tree is pretty deep we cannot use the process group to kill it, but we need to recurse on all the children.

In order to achieve this I used the `rkill` executable from the `pslist` package. In hindsight since we are only going to support linux, it would have been better to use the `unix` package for process and file management, and use haskell to walk the process tree.
