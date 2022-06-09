# derive-weak
Implements derive macro #[derive(Weak)] which creates ‘weak’ counterpart for the structure. I.e. when original structure contains reference counting pointers (Arc, Rc), the corresponding weak structure contains the weak variants of these pointers (std::sync::Weak, std::rc::Weak).

This may be useful when it’s inconvenient to store some data under refernce counting pointer, due to performance or ergonomic reasons.