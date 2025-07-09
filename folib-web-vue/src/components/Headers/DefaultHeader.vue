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
						<span>{{ $t('Headers.OfficialWebsite') }}</span>
					</span>

				</a-sub-menu>
				<a-sub-menu>
					<span slot="title" class="submenu-title-wrapper">
						<a-icon type="code" theme="filled" class="m-0" />
						<span>{{ $t('Headers.HelpDocumentation') }}</span>
					</span>

				</a-sub-menu>
				<a-sub-menu  >
					<span slot="title" class="submenu-title-wrapper">
						<a-icon type="eye" theme="filled" class="m-0" />
						<span>{{ $t('Headers.AboutUs') }}</span>
					</span>
					<a-menu-item>
						<router-link to="/">
							<span class="label">{{ $t('Headers.TeamIntroduction') }}</span>
						</router-link>
					</a-menu-item>
					<a-menu-item>
						<router-link to="/">
							<span class="label">{{ $t('Headers.TrialApplication') }}</span>
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
			<a-button size="small" type="dark" class="px-30 border-dark" shape="round" href="https://www.bocloud.com.cn/product/BeyondFolibrary" target="_blank" rel="noopenner noreferrer">{{ $t('Headers.PurchaseFullVersion') }}</a-button>
		</div>

    <div v-if="!(dalyOut||haveError)" class="header-col header-btn">
      <a-button size="small" type="dark" class="px-30 border-dark" shape="round" href="https://folib.com" target="_blank" rel="noopenner noreferrer">{{ $t('Headers.UserManual') }}</a-button>
    </div>
    <div v-if="!(dalyOut||haveError)" class="language-style">
      <a-dropdown :trigger="['click']" placement="bottomRight" :getPopupContainer="getContainer">
        <div class="table-avatar-info" id="language-svg">
          <svg t="1703685138742" class="icon" viewBox="0 0 1024 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="39012" width="20" height="20"><path d="M162.816 672h199.68c-5.472-40.192-8.96-83.2-10.08-128H129.28c3.744 45.344 15.36 88.48 33.504 128z m37.248 64a384.32 384.32 0 0 0 223.712 149.824c-20.64-38.272-37.92-89.6-50.368-149.824H200.064z m661.12-64c18.144-39.52 29.76-82.656 33.504-128h-223.104a1166.624 1166.624 0 0 1-10.08 128h199.68z m-37.248 64H650.56c-12.448 60.192-29.728 111.552-50.368 149.824A384.32 384.32 0 0 0 823.904 736z m-399.232-64h174.592c4.544-39.52 7.424-82.656 8.384-128h-191.36c0.96 45.344 3.84 88.48 8.384 128z m9.312 64c17.44 96.896 45.856 160 77.984 160 32.128 0 60.544-63.104 77.984-160h-155.968z m-304.704-256h223.104c1.12-44.8 4.608-87.808 10.08-128h-199.68a381.632 381.632 0 0 0-33.504 128z m765.376 0a381.632 381.632 0 0 0-33.504-128h-199.68c5.472 40.192 8.96 83.2 10.08 128h223.104zM416.32 480h191.36a1379.936 1379.936 0 0 0-8.384-128h-174.592a1379.936 1379.936 0 0 0-8.384 128zM200.064 288h173.344c12.448-60.192 29.728-111.552 50.368-149.824A384.32 384.32 0 0 0 200.096 288z m623.872 0a384.32 384.32 0 0 0-223.712-149.824c20.64 38.272 37.92 89.6 50.368 149.824h173.344zM434.016 288h155.968C572.544 191.104 544.128 128 512 128c-32.128 0-60.544 63.104-77.984 160zM512 960C264.576 960 64 759.424 64 512S264.576 64 512 64s448 200.576 448 448-200.576 448-448 448z" fill="#ffffff" p-id="39013"></path></svg>
        </div>
        <a-menu slot="overlay">
          <a-menu-item key="0" @click="changeLanguage('zh')">
            <a class="btn-sign-in">
              <div class="table-avatar-info">
                <div class="svg-style">
                  <svg t="1703595059282" class="icon" viewBox="0 0 1501 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="22719" width="22" height="22"><path d="M0.205 1003.52h1500.023V0.683H0.205z" fill="#DE2910" p-id="22720"></path><path d="M516.3 413.286l-2.184 33.724 31.403 12.425-32.768 8.465-2.185 33.723-18.022-28.672-32.905 8.397 21.573-25.941-18.023-28.672 31.54 12.493 21.572-25.942z m80.897-103.15l10.24 32.153h33.928l-27.306 19.934 10.24 32.154-27.307-20.07-27.307 19.933 10.513-32.085-27.306-19.798h33.86l10.445-32.222z m34.543-137.08l-15.292 30.174 23.893 23.893-33.382-5.325-15.497 30.174-5.188-33.587-33.45-5.188 30.242-15.36-5.257-33.383 23.757 23.894 30.174-15.292zM516.437 63.693l-2.048 33.655 31.403 12.493-32.836 8.465-2.185 33.656-18.09-28.672-32.7 8.465 21.572-25.942-18.022-28.672 31.402 12.493 21.504-25.941z m-267.605 51.063l31.676 97.622h102.81l-83.081 60.28 31.676 97.825-83.15-60.348-83.08 60.348 31.676-97.826-83.08-60.348h102.809l31.676-97.621z" fill="#FFDE00" p-id="22721"></path></svg>
                </div>
                 <span class="text-style">中文</span>
              </div>
            </a>
          </a-menu-item>
          <a-menu-item key="1" @click="changeLanguage('en')">
            <a class="btn-sign-in">
              <div class="table-avatar-info">
                <div class="svg-style">
                  <svg t="1703668600066" class="icon" viewBox="0 0 1536 1024" version="1.1" xmlns="http://www.w3.org/2000/svg" p-id="31283" width="22" height="22"><path d="M0 0h1536v1024H0V0z" fill="#00458F" p-id="31284"></path><path d="M1536 358.4H921.6V0h-307.2v358.4H0v307.2h614.4V1024h307.2V665.6H1536v-307.2z" fill="#FFFFFF" p-id="31285"></path><path d="M1536 905.6L176 0H0v118.4L1360 1024H1536v-118.4z" fill="#FFFFFF" p-id="31286"></path><path d="M0 905.6L1360 0H1536v118.4L176 1024H0v-118.4z" fill="#FFFFFF" p-id="31287"></path><path d="M1536 422.4H857.6V0h-179.2v422.4H0v179.2h678.4V1024h179.2V601.6H1536v-179.2z" fill="#CC3440" p-id="31288"></path><path d="M1536 1020.8v-80l-412.8-275.2h-121.6L1536 1020.8zM1536 0h-121.6L921.6 326.4v28.8h76.8L1536 0zM534.4 665.6L0 1020.8v3.2h115.2l496-329.6v-28.8h-76.8zM0 0v80l416 278.4h121.6L0 0z" fill="#CC3440" p-id="31289"></path></svg>
                </div>
                <span class="text-style">English</span>
              </div>
            </a>
          </a-menu-item>
        </a-menu>
      </a-dropdown>
    </div>
	</a-layout-header>
	<!-- / Layout Header ( Navbar ) -->

</template>

<script>
import {checkMachineCode, getServerName, getServerVersion} from "@/api/settings";
import {setLanguage} from "@/locale";

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
    computed: {
      getContainer() {
        return document.getElementById("language-svg")
      }
    },
    created() {
      getServerVersion().then(res=>{
        sessionStorage.setItem("instanceVersion",res)
      })
// TODO 这处代码要考虑在session过期之后不要在充分调用
      getServerName().then(res=>{
        this.instanceName=res
        sessionStorage.setItem("instanceName",res)
      })
	  sessionStorage.setItem("identityLevel",this.level)
    },
    methods: {
      changeLanguage(val) {
        setLanguage(val)
        this.$forceUpdate()
      },
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
  .language-style {
    top: 24px;
    position: absolute;
    right: -72px;
  }
  .text-style {
    line-height: 36px;
    font-weight: 700;
    font-size: 14px;
    margin-left: 12px;
    color: rgba(0, 0, 0, 0.65);
  }
  .svg-style {
    width: 36px;
    height: 36px;
    background-color: #FAFAFA;
    box-shadow: 0px 4px 6px rgba(0, 0, 0, 0.12);
    display: flex;
    justify-content: center;
    align-items: center;
  }
  ::v-deep.ant-dropdown-menu {
    min-width: 176px;
    border-radius: 12px;
    padding: 6px 4px;
  }
  ::v-deep.ant-dropdown-menu-item, .ant-dropdown-menu-submenu-title {
    padding: 6px 15px;
    color: #595959;
    margin: 8px 8px 12px;
  }
</style>
