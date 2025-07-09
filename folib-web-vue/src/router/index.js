import Vue from 'vue'
import VueRouter from 'vue-router'

import {getServerName} from "@/api/settings";

import storage from 'store'
import {ACCESS_TOKEN, USER_INFO} from '@/store/mutation-types'
import { encrypt } from "@/utils/jsencrypt"
import store from '@/store'
import Swal from 'sweetalert2'


Vue.use(VueRouter)

// 解决编程式路由往同一地址跳转时会报错的情况
const originalPush = VueRouter.prototype.push
const originalReplace = VueRouter.prototype.replace

// push
VueRouter.prototype.push = function push(location, onResolve, onReject) {
  if (onResolve || onReject)
    return originalPush.call(this, location, onResolve, onReject)
  return originalPush.call(this, location).catch(err => err)
}

//replace
VueRouter.prototype.replace = function push(location, onResolve, onReject) {
  if (onResolve || onReject)
    return originalReplace.call(this, location, onResolve, onReject)
  return originalReplace.call(this, location).catch(err => err)
}


let routes = [
	{
		// will match everything
		path: '*',
		component: () => import('../views/404.vue'),
	},
	{
		path: '/',
		name: 'Home',
		redirect: '/storages/home',
	},
	{
		path: '/storage/list',
		name: 'storages',
		layout: "dashboard",
		// route level code-splitting
		// this generates a separate chunk (about.[hash].js) for this route
		// which is lazy-loaded when the route is visited.
		meta: {
			title: 'router.RepositoryList',
			sidebarMap: ['router.RepositoryList'],
			breadcrumbs: ['router.ProductWarehouse', 'router.RepositoryList'],
		},
		component: () => import('../views/Storage/Storages.vue'),
	},
	{
		path: '/storages/home',
		name: 'storagesHome',
		layout: "dashboard",
		meta: {
			title: 'router.RepositoryList',
			layoutClass: 'layout-profile',
			sidebarMap: ['router.RepositoryList'],
			breadcrumbs: ['router.ProductWarehouse', 'router.RepositoryList'],
		},
		component: () => import('../views/Storage/Home.vue'),
	},

	{
		path: '/storage/list/libDetial',
		name: 'libDetial',
		layout: "dashboard",
		meta: {
			title: 'router.WarehouseBrowsing',
			layoutClass: 'layout-profile',
			sidebarMap: ['router.ProductWarehouse', 'router.RepositoryList', 'router.WarehouseBrowsing'],
			breadcrumbs: ['router.ProductWarehouse', 'router.RepositoryList', 'router.WarehouseBrowsing'],
		},
		component: () => import('../views/Storage/LibView.vue'),
	},
	{
		path: '/storage/scanner',
		name: 'scanner',
		layout: "dashboard",
		// route level code-splitting
		// this generates a separate chunk (about.[hash].js) for this route
		// which is lazy-loaded when the route is visited.
		meta: {
			title: 'router.ScanTheHomePage',
			sidebarMap: ['router.SecurityScanning'],
			breadcrumbs: ['router.SecurityScanning', 'router.ScanTheHomePage'],
		},
		component: () => import('../views/Storage/Scanner.vue'),
	},
	{
		path: '/storage/scanner/detial',
		name: 'scannerDetial',
		layout: "dashboard",
		// route level code-splitting
		// this generates a separate chunk (about.[hash].js) for this route
		// which is lazy-loaded when the route is visited.
		meta: {
			title: 'router.ScanDetails',
			sidebarMap: ['router.SecurityScanning','router.ScanDetails'],
			breadcrumbs: ['router.SecurityScanning', 'router.ScanTheHomePage','router.ScanDetails'],
		},
		component: () => import('../views/Storage/ScannerView.vue'),
	},
	{
		path: '/users',
		name: 'users',
		layout: "dashboard",
		meta: {
			title: 'router.UserManagement',
			sidebarMap: ['router.UserManagement', 'router.UserList'],
			breadcrumbs: ['router.UserManagement', 'router.UserList'],
		},
		component: () => import('../views/Users/Users.vue'),
	},
	{
		path: '/permissions',
		name: 'permissions',
		layout: "dashboard",
		meta: {
			title: 'router.Permissions',
			sidebarMap: ['router.Permissions', 'router.PermissionList'],
			breadcrumbs: ['router.Permissions', 'router.PermissionList'],
		},
		component: () => import('../views/Permissions/index.vue'),
	},
	{
		path: '/accessToken',
		name: 'accessToken',
		layout: "dashboard",
		meta: {
			title: 'router.AccessToken',
			sidebarMap: ['router.AccessToken', 'router.TokenList'],
			breadcrumbs: ['router.AccessToken', 'router.TokenList'],
		},
		component: () => import('../views/AccessToken/index.vue'),
	},
	{
		path: '/settings',
		name: 'settings',
		layout: "dashboard",
		meta: {
			title: 'router.GlobalSettings',
			sidebarMap: ['router.SetupManagement', 'router.GlobalSettings'],
			breadcrumbs: ['router.SetupManagement', 'router.GlobalSettings'],
		},
		component: () => import('../views/Setting/Settings.vue'),
	},
	{
		path: '/monitor',
		name: 'monitor',
		layout: "dashboard",
		meta: {
			title: 'router.HealthMonitoring',
			sidebarMap: ['router.SetupManagement', 'router.HealthMonitoring'],
			breadcrumbs: ['router.SetupManagement', 'router.HealthMonitoring'],
		},
		component: () => import('../views/Setting/Monitor.vue'),
	},
	{
		path: '/login',
		name: 'login',
		meta: {
			layoutClass: 'layout-sign-up-illustration',
			title: 'router.Login',
			sidebarMap: ['authentication', 'sign-up', 'illustration'],
			breadcrumbs: ['Authentication', 'Sign Up', 'Illustration'],
			nofooter: true,
		},
		component: () => import('../views/Authentication/sign-in/login.vue'),
	},

	{
		path: '/layout',
		name: 'Layout',
		layout: "dashboard",
		component: () => import('../views/Layout.vue'),
	},
	{
		path: '/personal',
		name: 'personal',
		layout: "dashboard",
		meta: {
			title: 'router.PersonalCenter',
			sidebarMap: ['router.PersonalCenter'],
			breadcrumbs: ['router.PersonalCenter'],
			nofooter: true,
		},
		component: () => import('../views/Users/Personal.vue'),
	},
	{
		path: '/advanced',
		name: 'advanced',
		layout: "dashboard",
		meta: {
			title: 'router.SeniorOperations',
			sidebarMap: ['router.SetupManagement', 'router.SeniorOperations'],
			breadcrumbs: ['router.SetupManagement', 'router.SeniorOperations'],
		},
		component: () => import('../views/Setting/Advanced.vue'),
	},
	/**
   * 开源治理相关
   */
  {
    path: "/artifacts",
    name: "artifacts",
    layout: "dashboard",
    meta: {
      title: 'router.ProductAnalysis',
      sidebarMap: ['router.OpenSourceGovernance', 'router.ProductAnalysis'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.ProductAnalysis'],
    },
    component: () => import("../views/ComponentAnalysis/Artifacts/index.vue"),
  },
  {
    path: "/artifacts/artifactsDetail",
    name: "artifactsDetail",
    layout: "dashboard",
    meta: {
      title: 'router.ProductDetails',
      sidebarMap: ['router.OpenSourceGovernance', 'router.ProductAnalysis', 'router.ProductDetails'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.ProductAnalysis', 'router.ProductDetails'],
    },
    component: () => import("../views/ComponentAnalysis/Artifacts/Detail.vue"),
  },
  {
    path: "/components",
    name: "components",
    layout: "dashboard",
    meta: {
      title: 'router.OpenSourceComponents',
      sidebarMap: ['router.OpenSourceGovernance', 'router.OpenSourceComponents'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.OpenSourceComponents'],
    },
    component: () => import("../views/ComponentAnalysis/Module/index.vue"),
  },
  {
    path: "/components/componentsDetail/:id",
    name: "componentsDetail",
    layout: "dashboard",
    meta: {
      title: 'router.ComponentDetails',
      sidebarMap: ['router.OpenSourceGovernance', 'router.OpenSourceComponents', 'router.ComponentDetails'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.OpenSourceComponents', 'router.ComponentDetails'],
    },
    component: () => import("../views/ComponentAnalysis/Module/Detail.vue"),
  },
  {
    path: "/vulnerabilities",
    name: "vulnerabilities",
    layout: "dashboard",
    meta: {
      title: 'router.VulnerabilityDatabase',
      sidebarMap: ['router.OpenSourceGovernance', 'router.VulnerabilityDatabase'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.VulnerabilityDatabase'],
    },
    component: () => import("../views/ComponentAnalysis/Vulnerabilities/index.vue"),
  },
  {
    path: "/vulnerabilities/vulnerabilitiesDetail/:id",
    name: "vulnerabilitiesDetail",
    layout: "dashboard",
    meta: {
      title: 'router.VulnerabilityDetails',
      sidebarMap: ['router.OpenSourceGovernance', 'router.VulnerabilityDatabase', 'router.VulnerabilityDetails'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.VulnerabilityDatabase', 'router.VulnerabilityDetails'],
    },
    component: () => import("../views/ComponentAnalysis/Vulnerabilities/Detail.vue"),
  },
  {
    path: "/licenses",
    name: "licenses",
    layout: "dashboard",
    meta: {
      title: 'router.CertificateStore',
      sidebarMap: ['router.OpenSourceGovernance', 'router.CertificateStore'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.CertificateStore'],
    },
    component: () => import("../views/ComponentAnalysis/Licenses/index.vue"),
  },
  {
    path: "/licenses/licensesDetail/:id",
    name: "licensesDetail",
    layout: "dashboard",
    meta: {
      title: 'router.CertificateDetails',
      sidebarMap: ['router.OpenSourceGovernance', 'router.CertificateStore', 'router.CertificateDetails'],
      breadcrumbs: ['router.OpenSourceGovernance', 'router.CertificateStore', 'router.CertificateDetails'],
    },
    component: () => import("../views/ComponentAnalysis/Licenses/Detail.vue"),
  }
]

// Adding layout property from each route to the meta
// object so it can be accessed later.
function addLayoutToRoute( route, parentLayout = "default" )
{
	route.meta = route.meta || {} ;
	route.meta.layout = route.layout || parentLayout ;

	if( route.children )
	{
		route.children = route.children.map( ( childRoute ) => addLayoutToRoute( childRoute, route.meta.layout ) ) ;
	}
	return route ;
}

let proLevel =  ["/artifacts", "/components", "/vulnerabilities", "/licenses"]

routes = routes.map( ( route ) => addLayoutToRoute( route ) ) ;

const router = new VueRouter({
	mode: 'hash',
	base: process.env.BASE_URL,
	routes,
	scrollBehavior (to, from, savedPosition) {
		if ( to.hash ) {
			return {
				selector: to.hash,
				behavior: 'smooth',
			}
		}
		return {
			x: 0,
			y: 0,
			behavior: 'smooth',
		}
	}
})

export default router
