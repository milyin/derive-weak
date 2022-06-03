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
