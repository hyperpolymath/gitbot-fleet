// SPDX-License-Identifier: PMPL-1.0-or-later
// Test fixture: file with modern, PQ-ready crypto for integration testing.
// This file intentionally uses approved algorithms.

fn hash_data(data: &[u8]) -> Vec<u8> {
    shake3_512::digest(data).to_vec()
}

fn fast_hash(data: &[u8]) -> Vec<u8> {
    blake3::hash(data).as_bytes().to_vec()
}

fn sign_message(sk: &SecretKey, msg: &[u8]) -> Signature {
    ml_dsa_87::sign(sk, msg)
}

fn key_exchange(pk: &PublicKey) -> SharedSecret {
    ml_kem_1024::encapsulate(pk)
}

fn encrypt(key: &[u8; 32], nonce: &[u8; 24], data: &[u8]) -> Vec<u8> {
    XChaCha20Poly1305::encrypt(key, nonce, data)
}

fn secure_random() -> [u8; 32] {
    let rng = ChaCha20Rng::from_seed(seed);
    rng.gen()
}
