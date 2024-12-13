import { axios } from '@/utils/request'



export function queryAllowlistDenylistBlock(data) {
    return axios({
        url: 'api/allowlistDenylistBlock/page',
        method: 'get',
        params: data
    })
}

export function addAllowlistDenylistBlock(data) {
    return axios({
        url: 'api/allowlistDenylistBlock',
        method: 'put',
        data: data
    })
}

export function updateAllowlistDenylistBlock(data) {
    return axios({
        url: 'api/allowlistDenylistBlock',
        method: 'post',
        data: data
    })
}

export function deleteAllowlistDenylistBlock(data) {
    return axios({
        url: 'api/allowlistDenylistBlock',
        method: 'delete',
        data: data
    })
}