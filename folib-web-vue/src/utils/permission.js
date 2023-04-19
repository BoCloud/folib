import store from '@/store'

export function isAnonymous() {
    if (store.state.user.token) {
        return false
    }
    return true
}

export function isLogin() {
    return !isAnonymous()
}

export function hasRole(role) {
    let roles = store.state.user.roles
    if (roles && roles.includes(role)) {
        return true
    }
    return false
}

export function isAdmin() {
    let roles = store.state.user.roles
    if (roles && roles.includes("ADMIN")) {
        return true
    }
    return false
}

export function hasPermission(permission) {
    let authorities = store.state.user.authorities
    if (authorities && authorities.includes(permission)) {
        return true
    }
    return false
}