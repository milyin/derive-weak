use derive_weak::Weak;

#[derive(Weak)]
#[weak_name(WeakFoo)]
struct Foo {
    foo: usize,
    #[weak_type(std::rc::Weak)]
    #[downgrade(std::rc::Rc::downgrade(&self.bar))]
    bar: std::rc::Rc<usize>,
}

#[test]
fn rc() {}
