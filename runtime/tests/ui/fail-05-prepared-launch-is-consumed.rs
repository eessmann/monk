// Expected E0382: prepared launch is consumed
use monk_runtime::capabilities::PreparedLaunch; pub fn bad(plan:PreparedLaunch){let _=plan.launch();let _=plan.launch();}
fn main() {}
