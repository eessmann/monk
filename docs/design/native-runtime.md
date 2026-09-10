# Native runtime ABI 1

Generated programs use native Fish where equivalence has been established and
`monk-runtime` for bounded byte and integer operations. The private Cabal
`monk-runtime-core` library has no ShellCheck dependency. It uses `bytestring`,
`process` and `unix`; Shelly's newline-adding `run` and separate `cd` state are
unsuitable for these observable boundaries. See the [Shelly API](https://hackage-content.haskell.org/package/shelly-1.12.1.1/docs/Shelly.html).

## Selection and transport

`monk-runtime --describe` reports `monk-runtime 1 bash53-i64-linux64`, followed
by space-separated capabilities. The profile requires 64-bit Linux. Translated
entries resolve their provider once, verify ABI/profile/required capabilities,
and preserve its absolute pathname as owned private state. `--runtime FILE`
selects a provider; otherwise lookup uses PATH. NUL-delimited path resolution
preserves embedded and trailing newlines. A provider must remain immutable and
compatible during execution. Missing/incompatible support returns 125 before
body effects, with a `monk.runtime` diagnostic.

Operations use `--abi 1 OPERATION`. Each input value ends in NUL; an empty stream
contains zero values and one NUL represents one empty value. Shell data cannot
contain NUL, but may contain invalid UTF-8. No operation evaluates supplied Bash,
Python, or Haskell expression strings. Protocol/arity failures return 125;
semantic statuses are operation-specific. Large values travel through pipes,
not process argument vectors. Metadata input never replaces the script's stdin.

| Family | Structured input and result |
| --- | --- |
| `integer` | An opcode and numeral operands, or a bounded postfix `batch` program of typed pure primitives. Result is three ASCII lines: `ok`, signed decimal, `-`; or `error`, `-`, first error identifier. |
| `split` | IFS and one byte value; output is zero or more framed fields. |
| `argv` | Prefix, suffix, empty-argv policy and positional values; output preserves argument cardinality and attachments. |
| `echo` | Arguments; output is Bash echo bytes, including option/escape handling and possible NUL output. |
| `pattern` / `glob` | Typed match/trim/literal replacement, or quote-activity/value pairs; results are status or framed byte values. No arbitrary regular expressions. |
| `descriptor-state` | No input; exit status is the original standard-descriptor bitmask. |
| `child-run` / `child-capture` | Warning origin, descriptor mask, owned generated Fish script, SHLVL and snapshot/argv frames. Original stdin arrives separately on owned fd3. |
| `directory-*` | Explicit bounded checks, physical cwd, stack formatting or conversion of actual parent Fish cd diagnostics. These operations do not change the parent's cwd. |

The integer specification uses unbounded Haskell intermediates with explicit
signed-64 wrapping after each primitive, truncation toward zero, bounded shift
counts, modular exponentiation and defined division edge cases. Both constant
folding and runtime execution use it. Batching excludes writes, lazy control and
failing division/remainder/power boundaries; errors remain in their execution
regions and retain source origins. Independent Bash comparisons validate the
shared specification rather than merely comparing its two consumers.

## Child ownership

Child stdout/stderr remain separate from control transport. Capture drains raw
bytes, reports the first NUL while draining, removes captured NULs, and strips
only trailing line feeds. Temporary script/state descriptors are unlinked,
owned, promoted above standard streams and verified through `/dev/fd`; other
private descriptors close before exec. Signal dispositions and termination
status are handled explicitly. Original closed standard descriptors are
reapplied around the child body after Fish startup.

A small C constructor records descriptor presence before GHC runtime startup
can reuse closed descriptor numbers. The operation implementations remain
Haskell. `-rtsopts=ignoreAll` prevents ambient `GHCRTS` from changing support
behavior or adding statistics to stderr; this is the documented
[GHC runtime option policy](https://downloads.haskell.org/ghc/9.14.1/docs/users_guide/runtime_control.html).

## Managed images and statistics

`--managed -o ENTRY` captures the selected provider bytes while planning,
validates the captured image, binds its generation-relative reference, and
materializes and admits the final output again. The opaque bundle exposes binary
image bytes, digest, required operations, ABI/profile, target and mode through
inspection functions. Arbitrary images cannot be forged by record updates.

Generation identity covers artifact role, path, mode and bytes. Runtime members
are staged at 0700, checked for compatibility and flushed before the entry
loader is atomically replaced. Reuse compares existing bytes and modes as data;
it never executes a possibly modified destination member. Existing loader,
recovery, publisher serialization and retained-generation rules still apply.
Children and exported functions keep the selected generation's absolute runtime
path, so later publication does not retarget previously installed functions.

Bundling includes one executable for the declared platform. The verification
report records its dynamic library dependencies; this is not a universally
portable static binary. Static materialization statistics count emitted helper
definitions, helper references, native operation call sites and Fish bytes,
including structural embedded child bodies. Measured launches, binary bytes,
bundle bytes and elapsed times are separate evidence.
