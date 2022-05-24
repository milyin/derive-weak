use derive_weak::Weak;

#[derive(Weak)]
struct Foo {
    foo: usize,
    bar: std::rc::Rc<usize>,
}

#[test]
fn rc() {}
