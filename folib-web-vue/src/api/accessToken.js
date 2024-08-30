import { axios } from '@/utils/request'


export function genToken(data) {
    return axios({
        url: '/api/accessToken',
        method: 'post',
        data,
    })
}

export function deleteToken(item) {
    return axios({
        url: '/api/accessToken',
        method: 'delete',
        params: item,
    })
}

export function tokenList(query) {
    return axios({
        url: '/api/accessToken',
        method: 'get',
        params: query,
    })
}