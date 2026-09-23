#![feature(pattern_types, pattern_type_macro)]
#![allow(incomplete_features, internal_features)]
mod native;
pub mod protocol;
pub mod semantics;
pub mod transport;
mod types;

pub mod abi2;
mod capsule;
mod child;
mod directory;
mod exec;
mod launch;
mod read;
mod session;

mod cli;

/// Safe descriptor and process capabilities used by the runtime.
/// Raw descriptor adoption and process-global signal authority remain private.
pub mod capabilities {
    pub use crate::native::{
        BorrowedDirectory, CompletedChild, PreparedLaunch, ProcessOutcome, RunningChild, Streams,
        WorkingDirectory,
    };
    pub use crate::types::{DescriptorMask, EndpointLease, SourceFd};
}

/// Run the native ABI command line and terminate with its resulting status.
pub fn run_cli() -> ! {
    cli::run()
}
