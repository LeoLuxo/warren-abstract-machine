use proc_macros::AutoDisplay;

#[derive(AutoDisplay)]
struct Test(i64);

#[derive(AutoDisplay)]
struct Test2 {
	field: i32,
}
