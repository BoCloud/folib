import request from '@/utils/request'

export function task () {
    return request({
      url: '/api/migrate/jfrog/task',
      method: 'get'
    })
  }
  export function updateTask (id, body) {
    return request({
      url: `/api/migrate/jfrog/task/${id}`,
      method: 'put',
      data: body
    })
  }

  export function addTask (body) {
    return request({
      url: '/api/migrate/jfrog/task',
      method: 'post',
      data: body
    })
  }

  export function getRepositories (params) {
    return request({
      url: '/api/migrate/jfrog/repository',
      method: 'get',
      params: params
    })
  }

  export function addMigrateRepo (body) {
    return request({
      url: '/api/migrate/jfrog/repository',
      method: 'post',
      data: body
    })
  }

  export function startMigrate (body) {
    return request({
      url: '/api/migrate/jfrog/start',
      method: 'post',
      data: body
    })
  }

  // 获取迁移进度 - 修改为批量获取
  export function getMigrateProgress(data) {
    return request({
      url: '/api/migrate/jfrog/repository/progress',
      method: 'post',
      data: data
    })
  }

  export function continueMigrate(data) {
    return request({
      url: '/api/migrate/jfrog/repository/continue',
      method: 'post',
      data: data
    })
  }
  
  export function finishMigrate(data) {
    return request({
      url: '/api/migrate/jfrog/repository/finish',
      method: 'post',
      data: data
    })
  }

  export function pauseMigrate(data) {
    return request({
      url: '/api/migrate/jfrog/repository/pause',
      method: 'post',
      data: data
    })
  }

  export function changeLayout(data) {
    return request({
      url: '/api/migrate/jfrog/repository/layout',
      method: 'put',
      data: data
    })
  }

  export function setFailed(data) {
    return request({
      url: '/api/migrate/jfrog/repository/failed',
      method: 'post',
      data: data
    })
  }

  export function getIndexProgress(data) {
    return request({
      url: '/api/migrate/jfrog/index/progress',
      method: 'post',
      data: data
    })
  }

export function getAllRepo(data) {
  return request({
    url: '/api/migrate/jfrog/repository/all',
    method: 'post',
    data: data
  })
}

export function deleteTask(id) {
  return request({
    url: '/api/migrate/jfrog/task/'+id,
    method: 'delete',
  })
}

export function restartMigrate(data) {
  return request({
    url: '/api/migrate/jfrog/repository/restart',
    method: 'post',
    data: data
  })
}

  