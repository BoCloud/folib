import { axios } from '@/utils/request'


export function getMetrics (ms) {
  return axios({
    url: '/api/monitoring/metrics/'+ms,
    method: 'get'
  })
}

export function getMetricsHealth () {
  return axios({
    url: '/api/monitoring/health',
    method: 'get'
  })
}

export function browseLogs (path) {
  if (!path) {
    path = ''
  }
  return axios({
    url: 'api/logging/browse/' + path,
    method: 'get',
    headers:{
      Accept:'application/json, text/plain, */*'
    }
  })
}

export function viewLogs (path) {
  return axios({
    url: '/api/logging/download/' + path,
    method: 'get',
    headers:{
      Accept:'*/*'
    }
  })
}



export function gremlinQuery (gremlin,sourceName) {
  return axios({
    url: '/api/gremlin/query',
    params: {
      gremlin: gremlin,
      sourceName: sourceName
    }
  })
}


export function gremlinVertex (nid,sourceName) {
  return axios({
    url: '/api/gremlin/vertex',
    params: {
      id: nid,
      sourceName: sourceName
    }
  })
}


export function gremlinEdge (eid,sourceName) {
  return axios({
    url: '/api/gremlin/edge',
    params: {
      id: eid,
      sourceName: sourceName
    }
  })
}

export function getCassandraClusterInfo () {
  return axios({
    url: '/api/node/cassandraClusterInfo',
    method: 'get'
  })
}

export function cassandraRemoveNode (token) {
  return axios({
    url: '/api/node/removeNode',
    method: 'put',
    data: {},
    params: {
      token: token
    }
  })
}

export function cassandraRepair () {
  return axios({
    url: '/api/node/repair',
    method: 'put',
    data: {}
  })
}