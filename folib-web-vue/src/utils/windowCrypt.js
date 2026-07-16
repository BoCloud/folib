const publicKey = 'MIGfMA0GCSqGIb3D'

export async function encryptData(data, aesKey = publicKey) {
    const encoder = new TextEncoder();
    const dataBuffer = encoder.encode(data);

    let keyBuffer;
    if (typeof aesKey === 'string') {
        const keyStr = aesKey.length >= 16 ? aesKey.slice(0, 16) : aesKey.padEnd(16, '0');
        keyBuffer
            = new TextEncoder().encode(keyStr);
    } else {
        keyBuffer
            = aesKey;
    }

    const cryptoKey = await window.crypto.subtle.importKey(
        'raw',
        keyBuffer,
        { name: 'AES-CBC' },
        false,
        ['encrypt', 'decrypt']
    );

    const iv = window.crypto.getRandomValues(new Uint8Array(16));
    const encryptedData = await window.crypto.subtle.encrypt(
        { name: 'AES-CBC', iv },
        cryptoKey,
        dataBuffer
    );

    return {
        iv: Array.from(iv),
        encryptedData: Array.from(new Uint8Array(encryptedData)),
    };
}

export async function decryptData(obj, key = publicKey) {
    const iv = new Uint8Array(obj.iv);
    const encryptedData = new Uint8Array(obj.encryptedData);

    const encoder = new TextEncoder();
    const keyBuffer = encoder.encode(key);

    const cryptoKey = await window.crypto.subtle.importKey(
        'raw',
        keyBuffer,
        { name: 'AES-CBC' },
        false,
        ['encrypt', 'decrypt']
    );

    const decryptedData = await window.crypto.subtle.decrypt(
        { name: 'AES-CBC', iv },
        cryptoKey,
        encryptedData
    );

    return new TextDecoder().decode(decryptedData);
}