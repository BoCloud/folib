import Vue from 'vue'
import VueRouter from 'vue-router'
import {
  getSsoList,
  addSsoClient,
  updateSsoClient,
  deleteClient

} from '@/api/sso'

import storage from 'store'
import {ACCESS_TOKEN, USER_INFO} from '@/store/mutation-types'




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
		redirect: '/anonymous/storages',
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
		path: '/anonymous/storages',
		name: 'anonymousStorages',
		layout: "dashboard",
		meta: {
			title: '仓库列表',
			layoutClass: 'layout-profile',
			sidebarMap: ['仓库列表'],
			breadcrumbs: ['制品仓库', '仓库列表'],
		},
		component: () => import('../views/Storage/AnonymousStorages.vue'),
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
	{
		path: '/advanced',
		name: 'advanced',
		layout: "dashboard",
		meta: {
			title: '高级运维',
			sidebarMap: ['设置管理', '高级运维'],
			breadcrumbs: ['设置管理', '高级运维'],
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
      title: "制品分析",
      sidebarMap: ["开源治理", "制品分析"],
      breadcrumbs: ["开源治理", "制品分析"],
    },
    component: () => import("../views/ComponentAnalysis/Artifacts/index.vue"),
  },
  {
    path: "/artifactsDetail/",
    name: "artifactsDetail",
    layout: "dashboard",
    meta: {
      title: "制品详情",
      sidebarMap: ["开源治理", "制品分析", "制品详情"],
      breadcrumbs: ["开源治理", "制品分析", "制品详情"],
    },
    component: () => import("../views/ComponentAnalysis/Artifacts/Detail.vue"),
  },
  {
    path: "/components",
    name: "components",
    layout: "dashboard",
    meta: {
      title: "开源组件",
      sidebarMap: ["开源治理", "开源组件"],
      breadcrumbs: ["开源治理", "开源组件"],
    },
    component: () => import("../views/ComponentAnalysis/Module/index.vue"),
  },
  {
    path: "/componentsDetail/:id",
    name: "componentsDetail",
    layout: "dashboard",
    meta: {
      title: "组件详情",
      sidebarMap: ["开源治理", "开源组件", "组件详情"],
      breadcrumbs: ["开源治理", "开源组件", "组件详情"],
    },
    component: () => import("../views/ComponentAnalysis/Module/Detail.vue"),
  },
  {
    path: "/vulnerabilities",
    name: "vulnerabilities",
    layout: "dashboard",
    meta: {
      title: "漏洞库",
      sidebarMap: ["开源治理", "漏洞库"],
      breadcrumbs: ["开源治理", "漏洞库"],
    },
    component: () => import("../views/ComponentAnalysis/Vulnerabilities/index.vue"),
  },
  {
    path: "/vulnerabilitiesDetail/:id",
    name: "vulnerabilitiesDetail",
    layout: "dashboard",
    meta: {
      title: "漏洞详情",
      sidebarMap: ["开源治理", "漏洞库", "漏洞详情"],
      breadcrumbs: ["开源治理", "漏洞库", "漏洞详情"],
    },
    component: () => import("../views/ComponentAnalysis/Vulnerabilities/Detail.vue"),
  },
  {
    path: "/licenses",
    name: "licenses",
    layout: "dashboard",
    meta: {
      title: "证书库",
      sidebarMap: ["开源治理", "证书库"],
      breadcrumbs: ["开源治理", "证书库"],
    },
    component: () => import("../views/ComponentAnalysis/Licenses/index.vue"),
  },
  {
    path: "/licensesDetail/:id",
    name: "licensesDetail",
    layout: "dashboard",
    meta: {
      title: "证书详情",
      sidebarMap: ["开源治理", "证书库", "证书详情"],
      breadcrumbs: ["开源治理", "证书库", "证书详情"],
    },
    component: () => import("../views/ComponentAnalysis/Licenses/Detail.vue"),
  },
  // {
  //   path: "/policy",
  //   name: "policy",
  //   layout: "dashboard",
  //   meta: {
  //     title: "策略管理",
  //     sidebarMap: ["开源治理", "策略管理"],
  //     breadcrumbs: ["开源治理", "策略管理"],
  //   },
  //   component: () => import("../views/ComponentAnalysis/Policy/index.vue"),
  // },
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


// 校验登录信息
router.beforeEach((from,to,next)=>{
  // 排除自己的登录页面
  if(from.path==='/'||from.path==='/anonymous/storages'||from.path==='/login'){
    next(true)
  }else{
  // 已登录直接跳转
  if (isLogin()) {
    next(true)
  // 没有跳转直接到登陆页面  
  } else {
    checkLoginInfo()
    next(false)
  }
}
 
})


// 单点登录的校验
async function checkLoginInfo(){
  // 首先要查到后端配置的单点配置信息
    let list=await getSsoList()
    // 本系统配置的clientId 否则不知道单点登录的页面地址在哪 后期考虑采用列表的 方式展现登录方式，目前先配死
    let clientId="single"
    let clientObject = list.filter(o=>o.clientId===clientId)[0]
    console.log(clientObject.ssoPath+"?redirectPath="+clientObject.redirectPath+"&clientId="+clientObject.clientId);

    let url =clientObject.ssoPath+"?redirect_uri="+clientObject.redirectPath+"&client_id="+clientObject.clientId+"&response_type=code"
    // 可以在输入的时候限定格式
    url= url.startsWith("http")? url:"http://"+url

    // 这可以选择登录的模式
    url = "https://www.keycloak.org/app/#url=http://localhost:8080&realm=myrealm&client=single"
    // 跳转到登陆页面
    window.location.href=url

}


  // 判断用户是否已经登录
  function isLogin() {
    let token= storage.get(ACCESS_TOKEN)
    return !!token   
  }

export default router
