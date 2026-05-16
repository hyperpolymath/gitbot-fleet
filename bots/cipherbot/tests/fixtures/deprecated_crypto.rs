// SPDX-License-Identifier: PMPL-1.0-or-later
// Test fixture: file with deprecated crypto for integration testing.
// This file intentionally contains weak/broken crypto references.

use md5;
use sha1;

fn hash_password(password: &str) -> String {
    let digest = md5::compute(password);
    format!("{:x}", digest)
}

fn verify_signature() {
    let hash = sha1::Sha1::new();
    let sig = Ed25519::sign(&sk, &msg);
    let des_cipher = des::Des::new(&key);
}

fn weak_rng() {
    let x = rand();
    srand(time(NULL));
}
