// Expected E0382: endpoint lease cannot be reused after transfer
use monk_runtime::capabilities::EndpointLease; pub fn bad(lease:EndpointLease){let _=lease.transfer();let _=lease.transfer();}
fn main() {}
