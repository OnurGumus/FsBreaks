# FsBreaks

Does a test fail when the rule it guards is switched off?

Coverage says a line ran. It doesn't say a test would notice if the line were wrong. FsBreaks answers that for the rules you care about. For each rule you write down a *break*: the smallest change to the code that switches the rule off. FsBreaks writes each break into the source in turn, builds, runs the tests that claim to guard the rule, and puts the file back. When every test still passes, the break is *missed*, which means one of two things: nothing guards the rule, or the code doesn't decide what you think it does.

It is mutation testing by intent. Tools like Stryker generate thousands of mutants and leave you to sort through the survivors. FsBreaks tries only the breaks you wrote, each named after its rule, so each one is worth reading. The breaks file sits next to the tests and reads like a list of the rules they guard.

```
dotnet tool install --global FsBreaks
```

## A breaks file

```
# The rules the booking tests guard.
test: tests/Booking.Tests --filter "Booking"

## A full class is not booked
src/Booking.fs
in Booking.book
- elif taken < seats then
+ elif true then

## A class that has started is not booked
src/Booking.fs
in Booking.book
- if now >= starts then
+ if false then
```

- **`## heading`** is the rule, in words. Name the rule, not the change.
- **The file** is the one the text is in, from the root.
- **`in Module.function`** (optional) is the binding the text is in, named by the end of its path: `book`, `Booking.book`, `Desk.Book` for a member, or `book.late` for a local `let`. The text then only needs to be found once in that binding rather than in the whole file. Anchors work only in F# files; any other file (C#, SQL, JSON) takes its breaks without one.
- **`-` lines** are the text as it is, and **`+` lines** are the break. Several lines make one text, indentation and all. A bare `-` or `+` is an empty line.
- **`test:`** names the project whose tests guard the file's breaks, followed by the arguments its tests run with. `{name}` in an argument is replaced by the breaks file's name.
- **`guarded by <project> [args]`**, under a break, gives that one break other tests, for a rule the file's tests cannot stage.
- Lines starting with `#` are notes.

A break's text has to be found exactly once. When the code moves on and the text is found more or fewer times, the break is reported as *stale* and is not tried.

## Commands

```
fsbreaks [run] [<name>...] [--only <text>]
fsbreaks check [<name>...] [--only <text>]
fsbreaks suggest <file> [<binding>]
```

**`run`** tries each break. First it builds each break's tests and runs them unbroken: a break only counts as caught against tests that were passing. Then, for each break, it writes the break, builds, runs the tests, and puts the file back byte for byte. It exits with 1 if any break is missed, stale or doesn't build.

```
booking
  caught   A full class is not booked
           by a full class is not booked
  MISSED   A class that has started is not booked
           every test in tests/Booking.Tests still passes

2 breaks tried: 1 caught, 1 missed, 0 stale, 0 not building.
```

If a run is stopped with Ctrl-C, the file is put back on the way out. If it is killed outright, the next `fsbreaks` puts it back from the copy it keeps in `.fsbreaks/`.

**`check`** finds each break's text and type-checks the file with the break in it, inside its project, with the F# compiler service. It doesn't build or run anything, so it takes seconds rather than a build per break. Run it while writing breaks, or in CI on every change, to catch stale breaks early. Restore the projects first. It checks only the file the break is in, so a break that changes a signature other files use gets past `check` and fails at `run`'s build.

**`suggest`** reads a file, or one binding in it, and prints candidate breaks in the breaks file format:

- each `if`/`elif` condition, as `false`;
- each `when` guard, as `false`;
- each requirement of an `&&` chain, dropped one at a time (as `true`), and each alternative of an `||` chain (as `false`);
- a comparison or a `not` that stands alone, turned around.

```
$ fsbreaks suggest src/Booking.fs book

## book: now >= starts → false
# line 12
src/Booking.fs
in book
- now >= starts
+ false
```

Keep the candidates that break a rule your tests should catch, and rename each heading to the rule. Suggestions are a starting point. The breaks worth keeping are often ones no generator writes: a SQL `WHERE` clause widened, a deadline counted from the wrong day, an email not sent.

## Settings

`fsbreaks.json`, in the folder you run from or the nearest one above it. Every setting is optional:

```jsonc
{
  "breaks": "test/Breaks",          // the folder of the .breaks files (default: breaks)
  "test": "tests/Rules.Tests",      // tests for a breaks file without a test: line
  "env": { "TESTS_DRIVER": "fast" },// environment for the builds and tests
  "timeoutMinutes": 15              // past this the tests count as hanging, and the break as caught
}
```

The folder the settings file is in is the root, and all paths are relative to it.

## How tests are run

A test project that sets `<OutputType>Exe</OutputType>` is run with `dotnet run --project <project> --no-build -- <args>`. That covers Expecto and Microsoft.Testing.Platform projects. Any other project is run with `dotnet test <project> --no-build <args>`. The exit code decides whether the tests passed. The failed tests are named from Expecto's, `dotnet test`'s and Microsoft.Testing.Platform's output, without any leading part that one of the guard's arguments already says, such as the filter.

A filter that matches no test is reported, rather than every break being missed.

## Writing good breaks

- **One rule per break, and name it.** "A full class is not booked" tells the next reader what the tests promise. "Changed `<` to `<=`" doesn't.
- **The smallest change that switches the rule off.** Turn a condition to `false`, drop one requirement, remove a `WHERE` clause. A break that crashes everything is caught by any test and proves nothing.
- **Keep them next to the tests that guard them**, with one breaks file per feature or test file, and run the file's own tests rather than all of them. It is faster, and it proves those tests are the ones that guard the rule.
- **A missed break is a finding.** Either write the test that catches it, or delete the code that decided nothing.

## License

MIT
