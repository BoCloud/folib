import { axios } from '@/utils/request'


export function getUsers () {
  return axios({
    url: '/api/users',
    method: 'get'
  })
}
export function getUserDetial (username) {
  return axios({
    url: '/api/users/'+username+'?formFields=true',
    method: 'get'
  })
}

export function putUserDetial (user) {
  return axios({
    url: '/api/users/'+user.username,
    method: 'put',
    data: user
  })
}
export function getUsersCreateFields () {
  return axios({
    url: '/api/formData/userFields',
    method: 'get'
  })
}
export function delUser (username) {
  return axios({
    url: '/api/users/'+username,
    method: 'delete'
  })
}






