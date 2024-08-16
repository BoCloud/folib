import { axios } from '@/utils/request'


// 获取审计日志列表
export function getAuditLogList(data) {
    return axios({
        url: '/api/audit/log',
        method: 'post',
        data,
    })
}

// 获取模块列表
export function getModuleList() {
    return axios({
        url: '/api/audit/module',
        method: 'get',
    })
}


//根据模块获取事件列表
export function getEventByModule(module) {
    return axios({
        url: '/api/audit/event/'+module,
        method: 'get',
    })
}

//获取所有事件列表
export function getEvents(module) {
    return axios({
        url: '/api/audit/event',
        method: 'get',
    })
}

//更新事件
export function updateEvent(events) {
    return axios({
        url: '/api/audit/event',
        method: 'post',
        data: events,
    })
}



