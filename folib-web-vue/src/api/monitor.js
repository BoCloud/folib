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


export function viewLogs () {
  return axios({
    url: '/api/logging/download/folib.log',
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