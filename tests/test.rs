use derive_weak::Weak;

#[derive(Weak)]
#[weak_name(WeakFoo)]
struct Foo {
    foo: usize,
    bar: std::rc::Rc<usize>,
}

#[test]
fn rc() {}
