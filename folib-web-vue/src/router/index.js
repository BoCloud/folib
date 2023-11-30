import Vue from 'vue'
import VueRouter from 'vue-router'
import {
  getSsoList,
  ssoLogin,
  getToken

} from '@/api/sso'

import {getServerName} from "@/api/settings";

import storage from 'store'
import {ACCESS_TOKEN, USER_INFO} from '@/store/mutation-types'
import { encrypt } from "@/utils/jsencrypt"
import store from '@/store'
import Swal from 'sweetalert2'


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
		path: '/storage/list/libDetial',
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
    path: "/artifacts/artifactsDetail",
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
    path: "/components/componentsDetail/:id",
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
    path: "/vulnerabilities/vulnerabilitiesDetail/:id",
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
    path: "/licenses/licensesDetail/:id",
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


// 校验登录信息
router.beforeEach((to,from,next)=>{

  getServerName().then(res=>{
    sessionStorage.setItem("instanceName",res)
  })
	let identityLevel = sessionStorage.getItem("identityLevel")
	if (proLevel.includes(to.path) && identityLevel !== 'pro') {
		Swal.fire({
			title: '提示信息',
			text: '此功能为高级版尊享，如需体验，请升级为高级版',
			confirmButtonColor: '#1890ff',
			confirmButtonText: '好的'
		})
		next(false)
		return
	}

  // todo 校验合法性 keyClock确定登录的合法性，方式仿冒登录
  let flag = sessionStorage.getItem("loginMethod")
  if(flag==="single"){
   // 判断单点是否已经登录
   if(sessionStorage.getItem("loginStatus")==="on"){
    // 校验单点登录授权是都合法
    next(true)
   }else{
  // 如果没有登录，则进行登录操作 如果已经登录则需要校验登录的合法性
    let param = window.location.search.substring(1).split("&")
    let clientInfo=JSON.parse(sessionStorage.getItem("clientInfo"))
    let sessionParam={
          grantType:"authorization_code",
          clientId:clientInfo.clientId,
          redirectUri:clientInfo.redirectPath,
          accessTokenUrl:clientInfo.accessTokenUrl
    }
   
    param.forEach(e=>{
      let temp = e.split("=")
      sessionParam[temp[0]]=temp[1]||""
    })

    if(!sessionParam.code){
      next(true)
      return
    }    
     //  这是从单点登录的页面跳转过来的
       ssoLogin(sessionParam).then(res=>{
        //在这里获取accessToken
        let password = encrypt("guest")
        let user = {
          username:res.username,
          password:password
        }
        sessionStorage.setItem("loginStatus","on")
        store.dispatch("Login", user).then((res) => {
          if (res.token != null) {
            store.dispatch("GetInfo").then((res) => {
            })
          }  
  
       })    
      })
   }
}
  next(true)
})



export default router
