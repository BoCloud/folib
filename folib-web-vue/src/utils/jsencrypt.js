import JSEncrypt from 'jsencrypt'

const encryptor = new JSEncrypt()
const publicKey = 'MIGfMA0GCSqGSIb3DQEBAQUAA4GNADCBiQKBgQCMrv98/OzupfeKSK4nCIKNVaWLzbiv0rXZXoSGkDOkfT5gwK+f6YZxWFiRIfmYTFCJenP6iL3EK9MtENhUTxetPOZeEC72ySPhUPcUrKSlhRhM1LqBFaIDrY5Qc9BZd1VztMJmUqwp1IRdeD1lgTc+XtF9WSFRK3hlo+F6wkfjYwIDAQAB'
encryptor.setPublicKey(publicKey)

export function encrypt(content) {
  return encryptor.encrypt(content)
}
