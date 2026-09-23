# Comparison inputs

`comparison-corpus.json` owns the ordered 95-fixture compatibility corpus, its
source hashes, argv/metadata and input bytes; the fixed common16 selector; and
the identities of three unavailable arithmetic inputs. It contains no measured
translation outcomes, observations or timings. The collector pins the manifest
digest so changes to its membership or execution metadata require an explicit
corpus update, rather than silently redefining a comparison.

`frozen95/background-jobs.bash` preserves the original corpus bytes separately
from the evolving integration fixture. The freezer copies those bytes over the
active fixture only inside its new input snapshot.

The arithmetic3 files are absent. Their neutral paths identify where original
bytes could be restored; no replacement sources are generated. A performance
freeze reports each as unavailable unless a file with its exact recorded hash
is present. These definitions do not establish current performance acceptance.

Historical accepted-outcome sets are not stored here. Legacy native collector
gates therefore make no claim about retaining those retired outcomes; current
admission and equivalence must be checked against the supplied providers.
