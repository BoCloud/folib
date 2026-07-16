const getters = {
  token: state => state.user.token,
  avatar: state => state.user.avatar,
  name: state => state.user.name,
  securityTokenKey: state => state.user.securityTokenKey,
  roles: state => state.user.roles,
  userInfo: state => state.user.info,
  enabled: state => state.user.enabled
}

export default getters
