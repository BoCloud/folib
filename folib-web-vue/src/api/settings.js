import { axios } from '@/utils/request'


export function getServerSettings () {
  return axios({
    url: '/api/configuration/folib/serverSettings',
    method: 'get'
  })
}
export function postServerSettings (data) {
  return axios({
    url: '/api/configuration/folib/serverSettings',
    method: 'post',
    data:data
  })
}

export function getLdap () {
  return axios({
    url: '/api/configuration/ldap',
    method: 'get'
  })
}
export function putLdap (data) {
  return axios({
    url: '/api/configuration/ldap',
    method: 'put',
    data:data
  })
}
export function getCrontasks () {
  return axios({
    url: '/api/configuration/crontasks/',
    method: 'get'
  })
}
export function delUser (username) {
  return axios({
    url: '/api/users/'+username,
    method: 'delete'
  })
}

export function getMachineCode () {
  return axios({
    url: '/api/ping/machineCode',
    method: 'get'
  })
}

export function postActivate (key,isPoc) {
  return axios({
    url: '/api/ping/activate?key='+key+'&isPoc='+isPoc,
    method: 'get'
  })
}

export function checkMachineCode () {
  return axios({
    url: '/api/ping/checkMachineCode',
    method: 'get'
  })
}

export function getMetadataConfiguration () {
  return axios({
    url: '/api/artifact/getMetadataConfiguration',
    method: 'get'
  })
}

export function globalSettingAddOrUpdateMetadata (data) {
  return axios({
    url: '/api/artifact/globalSettingAddOrUpdateMetadata',
    method: 'put',
    data: data
  })
}

export function globalSettingDeleteMetadata (data) {
  return axios({
    url: '/api/artifact/globalSettingDeleteMetadata',
    method: 'delete',
    data: data
  })
}

export function getArtifactDispatchConfig(){
  return axios({
    url: '/api/configuration/folib/dispatch',
    method: 'get'
  })
}

export function globalSettingArtifactDispatchConfig (data) {
  return axios({
    url: '/api/configuration/folib/dispatch',
    method: 'put',
    data: data
  })
}

export function globalSettingDelArtifactDispatchConfig(clusterEnName) {
  return axios({
    url: '/api/configuration/folib/dispatch/' + clusterEnName,
    method: 'delete'
  })
}

export function vulnerabilitiesDataUpdate() {
  return axios({
    url: '/api/folibScanner/update',
    method: 'get'
  })
}