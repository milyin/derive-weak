use derive_weak::Weak;

#[test]
fn rc() {
    #[derive(PartialEq, Weak)]
    #[weak(WeakFoo)]
    struct Foo {
        foo: usize,
        #[weak(std::rc::Weak)]
        #[downgrade(std::rc::Rc::downgrade(&self.bar))]
        #[upgrade(self.bar.upgrade())]
        bar: std::rc::Rc<usize>,
    }

    let foo = Foo {
        foo: 42,
        bar: std::rc::Rc::new(42),
    };
    let wfoo = foo.downgrade();
    let foo2 = wfoo.upgrade().unwrap();
    assert!(foo == foo2);
    drop(foo);
    drop(foo2);
    assert!(wfoo.upgrade().is_none());
}
