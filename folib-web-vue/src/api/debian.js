import { axios } from '@/utils/request'


export function parseArtifact(formData, onUploadProgress) {
    return axios({
        url: '/api/debian/parseArtifact',
        method: 'post',
        timeout: 15 * 60 * 1000,
        headers: { "Content-type": "multipart/form-data", },
        data: formData,
        onUploadProgress,
    });
}

export function uploadArt(data) {
    return axios({
        url: '/api/debian/upload',
        method: 'post',
        data: data,
    })
}

export function uploadBatchArt(formData) {
    return axios({
        url: '/api/debian/batchUpload',
        method: 'post',
        headers: { "Content-type": "multipart/form-data", },
        data: formData,
    });
}
