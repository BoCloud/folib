import Vue from 'vue'
import VueRouter from 'vue-router'

Vue.use(VueRouter)

let routes = [
	{
		// will match everything
		path: '*',
		component: () => import('../views/404.vue'),
	},
	{
		path: '/',
		name: 'Home',
		redirect: '/login',
	},
	{
		path: '/dashboards/',
		name: 'Dashboard',
		layout: "dashboard",
		// route level code-splitting
		// this generates a separate chunk (about.[hash].js) for this route
		// which is lazy-loaded when the route is visited.
		meta: {
			title: '存储分析',
			sidebarMap: ['dashboards'],
			breadcrumbs: ['首页', '存储分析'],
		},
		component: () => import(/* webpackChunkName: "dashboard" */ '../views/Dashboards/Default.vue'),
	},
	{
		path: '/dashboards/crm',
		name: 'DashboardsCRM',
		layout: "dashboard",
		meta: {
			title: 'CRM',
			sidebarMap: ['dashboards'],
			breadcrumbs: ['Dashboards', 'CRM'],
		},
		component: () => import('../views/Dashboards/CRM.vue'),
	},
	{
		path: '/storage/list',
		name: 'storages',
		layout: "dashboard",
		// route level code-splitting
		// this generates a separate chunk (about.[hash].js) for this route
		// which is lazy-loaded when the route is visited.
		meta: {
			title: '仓库列表',
			sidebarMap: ['仓库列表'],
			breadcrumbs: ['制品仓库', '仓库列表'],
		},
		component: () => import('../views/Storage/Storages.vue'),
	},

	{
		path: '/storage/libDetial',
		name: 'libDetial',
		layout: "dashboard",
		meta: {
			title: '仓库浏览',
			layoutClass: 'layout-profile',
			sidebarMap: ['制品仓库', '仓库列表', '仓库浏览'],
			breadcrumbs: ['制品仓库', '仓库列表', '仓库浏览'],
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
			title: '扫描首页',
			sidebarMap: ['安全扫描'],
			breadcrumbs: ['安全扫描', '扫描首页'],
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
			title: '扫描详情',
			sidebarMap: ['安全扫描','扫描详情'],
			breadcrumbs: ['安全扫描', '扫描首页','扫描详情'],
		},
		component: () => import('../views/Storage/ScannerView.vue'),
	},
	{
		path: '/users',
		name: 'users',
		layout: "dashboard",
		meta: {
			title: '用户管理',
			sidebarMap: ['用户管理', '用户列表'],
			breadcrumbs: ['用户管理', '用户列表'],
		},
		component: () => import('../views/Users/Users.vue'),
	},
	{
		path: '/settings',
		name: 'settings',
		layout: "dashboard",
		meta: {
			title: '全局设置',
			sidebarMap: ['设置管理', '全局设置'],
			breadcrumbs: ['设置管理', '全局设置'],
		},
		component: () => import('../views/Setting/Settings.vue'),
	},
	{
		path: '/monitor',
		name: 'monitor',
		layout: "dashboard",
		meta: {
			title: '健康监测',
			sidebarMap: ['设置管理', '健康监测'],
			breadcrumbs: ['设置管理', '健康监测'],
		},
		component: () => import('../views/Setting/Monitor.vue'),
	},
	{
		path: '/login',
		name: 'login',
		meta: {
			layoutClass: 'layout-sign-up-illustration',
			title: '登录',
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
			title: '个人中心',
			sidebarMap: ['个人中心'],
			breadcrumbs: ['个人中心'],
			nofooter: true,
		},
		component: () => import('../views/Users/Personal.vue'),
	},
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
