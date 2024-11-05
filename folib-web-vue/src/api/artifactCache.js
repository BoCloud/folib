import { axios } from '@/utils/request'

export function cleanupArtifactCacheDirectory(query) {
  return axios({
    url: '/api/artifactCache/cleanupArtifactCacheDirectory',
    method: 'delete',
    params: query
  })
}

export function artifactCacheDirectoryUseSize(query) {
  return axios({
    url: '/api/artifactCache/artifactCacheDirectoryUseSize',
    method: 'get',
    params: query
  })
}