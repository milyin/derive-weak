use async_object::{CArc, EArc};
use std::{rc::Rc, sync::Arc};

use derive_weak::Weak;

#[test]
fn auto() {
    #[derive(PartialEq, Weak)]
    struct Foo {
        foo: usize,
        rc: Rc<usize>,
        arc: Arc<usize>,
        carc: CArc<usize>,
        earc: EArc,
    }
    let foo = Foo {
        foo: 42,
        rc: Rc::new(42),
        arc: Arc::new(42),
        carc: CArc::new(42),
        earc: EArc::new(),
    };
    let wfoo: WFoo = foo.downgrade();
    let std::rc::Weak { .. } = wfoo.rc;
    let std::sync::Weak { .. } = wfoo.arc;
    let async_object::WCArc { .. } = wfoo.carc;
    let async_object::WEArc { .. } = wfoo.earc;
    let foo2: Foo = wfoo.upgrade().unwrap();
    assert!(foo == foo2);
    drop(foo);
    drop(foo2);
    assert!(wfoo.upgrade().is_none());
}

#[test]
fn no_auto() {
    #[derive(PartialEq, Weak)]
    #[weak(name=WeakFoo, auto=false)]
    struct Foo {
        foo: usize,
        #[weak]
        rc: Rc<usize>,
        #[weak]
        arc: Arc<usize>,
        #[weak]
        carc: CArc<usize>,
        #[weak]
        earc: EArc,
        // #[weak]
        unsupported: (),
    }
    let foo = Foo {
        foo: 42,
        rc: Rc::new(42),
        arc: Arc::new(42),
        carc: CArc::new(42),
        earc: EArc::new(),
        unsupported: (),
    };
    let wfoo: WeakFoo = foo.downgrade();
    let std::rc::Weak { .. } = wfoo.rc;
    let std::sync::Weak { .. } = wfoo.arc;
    let async_object::WCArc { .. } = wfoo.carc;
    let async_object::WEArc { .. } = wfoo.earc;
    let foo2: Foo = wfoo.upgrade().unwrap();
    assert!(foo == foo2);
    drop(foo);
    drop(foo2);
    assert!(wfoo.upgrade().is_none());
}

#[test]
fn implicit() {
    #[derive(PartialEq, Weak, Clone)]
    struct Foo {
        rc: Rc<usize>,
    }
    #[derive(PartialEq, Weak)]
    struct Bar {
        #[weak]
        foo: Foo,
    }
    let bar = Bar {
        foo: Foo { rc: Rc::new(42) },
    };
    let wbar: WBar = bar.downgrade();
    let WFoo { .. } = wbar.foo;
    let std::rc::Weak { .. } = wbar.foo.rc;
    let bar2: Bar = wbar.upgrade().unwrap();
    assert!(bar2 == bar);
    drop(bar);
    drop(bar2);
    assert!(wbar.upgrade().is_none());
}

#[test]
fn explicit() {
    #[derive(PartialEq, Weak, Clone)]
    #[weak(name=WeakFoo)]
    struct Foo {
        rc: Rc<usize>,
    }
    #[derive(PartialEq, Weak)]
    struct Bar {
        #[weak(name=WeakFoo, upgrade=_.upgrade(), downgrade=_.downgrade())]
        foo: Foo,
    }
    let bar = Bar {
        foo: Foo { rc: Rc::new(42) },
    };
    let wbar: WBar = bar.downgrade();
    let WeakFoo { .. } = wbar.foo;
    let std::rc::Weak { .. } = wbar.foo.rc;
    let bar2: Bar = wbar.upgrade().unwrap();
    assert!(bar2 == bar);
    drop(bar);
    drop(bar2);
    assert!(wbar.upgrade().is_none());
}

// #[test]
// fn clone() {
//     #[derive(Weak, Clone)]
//     struct Foo {
//         rc: Rc<usize>,
//     }
//     let foo = Foo { rc: Rc::new(42) };
//     let wfoo = foo.downgrade();
//     let wfoo2 = wfoo.clone();
// }
