import { axios } from '@/utils/request'



export function federalPromotionPolicyQuery(data) {
    return axios({
        url: '/api/federal/promotion/policy/list',
        method: 'get',
        params: data
    })
}

export function federalPromotionPolicyAdd(data) {
    return axios({
        url: '/api/federal/promotion/policy',
        method: 'put',
        data: data
    })
}

export function federalPromotionPolicyEdit(data) {
    return axios({
        url: '/api/federal/promotion/policy',
        method: 'post',
        data: data
    })
}

export function federalPromotionPolicyDetail(data) {
    return axios({
        url: '/api/federal/promotion/policy/'+data,
        method: 'get'
    })
}

export function federalPromotionPolicyDelete(data) {
    return axios({
        url: '/api/federal/promotion/policy/'+data,
        method: 'delete'
    })
}