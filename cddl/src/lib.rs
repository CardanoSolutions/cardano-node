use cddl::validate_cbor_from_slice;
use std::{slice, str};

#[no_mangle]
pub extern "C" fn validate_cbor(
    cddl_ptr: *const u8,
    cddl_len: usize,
    cbor_ptr: *const u8,
    cbor_len: usize,
) -> () {
    let cddl = unsafe { slice::from_raw_parts(cddl_ptr, cddl_len) };
    let cbor = unsafe { slice::from_raw_parts(cbor_ptr, cbor_len) };
    match validate_cbor_from_slice(str::from_utf8(cddl).unwrap(), cbor, None) {
        Ok(_) => (),
        Err(e) => panic!("{e:#?}"),
    }
}
