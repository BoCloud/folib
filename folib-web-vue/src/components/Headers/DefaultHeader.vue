<template>

	<!-- Layout Header ( Navbar ) -->
	<a-layout-header>
		<div class="header-col header-brand">
			<h6>{{ instanceName }}</h6>

			<!-- Trigger Button For Navigation Menu For Small Screens -->
			<a-button type="link" @click="collapseNav = collapseNav ? 0 : 1 " class="btn-menu-trigger">
				<svg width="20" height="20" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 448 512"><path d="M16 132h416c8.837 0 16-7.163 16-16V76c0-8.837-7.163-16-16-16H16C7.163 60 0 67.163 0 76v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16zm0 160h416c8.837 0 16-7.163 16-16v-40c0-8.837-7.163-16-16-16H16c-8.837 0-16 7.163-16 16v40c0 8.837 7.163 16 16 16z"/></svg>
			</a-button>
			<!-- Trigger Button For Navigation Menu For Small Screens -->

		</div>
		<div class="header-col header-nav">

			<!-- Navigation Menu For Large Screens -->
			<a-menu v-if="dalyOut||haveError" mode="horizontal" class="menu-large">
				<a-sub-menu >
					<span slot="title" class="submenu-title-wrapper">
						<a-icon type="home" theme="filled" class="m-0" />
						<span>官网</span>
					</span>

				</a-sub-menu>
				<a-sub-menu>
					<span slot="title" class="submenu-title-wrapper">
						<a-icon type="code" theme="filled" class="m-0" />
						<span>帮助文档</span>
					</span>

				</a-sub-menu>
				<a-sub-menu  >
					<span slot="title" class="submenu-title-wrapper">
						<a-icon type="eye" theme="filled" class="m-0" />
						<span>关于我们</span>
					</span>
					<a-menu-item>
						<router-link to="/">
							<span class="label">团队介绍</span>
						</router-link>
					</a-menu-item>
					<a-menu-item>
						<router-link to="/">
							<span class="label">试用申请</span>
						</router-link>
					</a-menu-item>
				</a-sub-menu>
			</a-menu>
			<!-- / Navigation Menu For Large Screens -->

			<!-- Collapsible Navigation Menu For Small Screens -->
			<div class="menu-small">

			</div>
			<!-- / Collapsible Navigation Menu For Small Screens -->

		</div>
		<div v-if="dalyOut||haveError" class="header-col header-btn">
			<a-button size="small" type="dark" class="px-30 border-dark" shape="round" href="http://folib.com" target="_blank">购买正式版</a-button>
		</div>

    <div v-if="!(dalyOut||haveError)" class="header-col header-btn">
      <a-button size="small" type="dark" class="px-30 border-dark" shape="round" href="http://folib.com/core/use.html" target="_blank">使用文档</a-button>
    </div>
	</a-layout-header>
	<!-- / Layout Header ( Navbar ) -->

</template>

<script>
import {checkMachineCode,getServerName} from "@/api/settings";


	export default ({
		data() {
			return {
				// Collapse navigation value.
				// Binded model property for "Collapsible Navigation Menu" collapsed status .
				collapseNav: 0,
				// Sidebar collapsed status.
				sidebarCollapsed: {
					type: Boolean,
					default: false,
				},
        haveError: false,
        dalyOut: false,
				level: 'basic',
				// Main sidebar color.
				sidebarColor: {
					type: String,
					default: "primary",
				},
				
				// Main sidebar theme : light, white, dark.
				sidebarTheme: {
					type: String,
					default: "light",
				},
      			rootSubmenuKeys: ['dashboards', 'pages', 'applications', 'ecommerce', 'authentication', 'basic', 'components', 'changelog'],
				openKeys: null,
        instanceName:sessionStorage.getItem("instanceName")||""
			}
		},
    created() {

      getServerName().then(res=>{
        this.instanceName=res
        sessionStorage.setItem("instanceName",res)
      })
	  sessionStorage.setItem("identityLevel",this.level)
      checkMachineCode().then(res=>{
        this.haveError=res.haveError
        this.dalyOut=res.dalyOut
				this.level = res.level
				sessionStorage.setItem("identityLevel",this.level)
      })
    },
    methods: {
			onOpenChange(openKeys)
			{
				this.openKeys = this.openKeys ? this.openKeys : this.$route.meta.sidebarMap ;
				
				const latestOpenKey = openKeys.find( key => this.openKeys.indexOf( key ) === -1) ;

				if ( this.rootSubmenuKeys.indexOf( latestOpenKey ) === -1 )
				{
					this.openKeys = openKeys;
				}
				else
				{
					this.openKeys = latestOpenKey ? [ latestOpenKey ] : [] ;
				}
			},
		},
	})

</script>

<style lang="scss" scoped>

	.nav-link svg {
		margin-right: 5px;
		vertical-align: middle;
	}
	.nav-link span {
		vertical-align: middle;
	}
	.ant-menu-submenu-popup {
		width: 100%;
	}

</style>
