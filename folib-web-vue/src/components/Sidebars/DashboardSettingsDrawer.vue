<template>

	<!-- Settings Drawer -->
	<a-drawer
		class="settings-drawer"
		:class="[ rtl ? 'settings-drawer-rtl' : '' ]"
		:placement="rtl ? 'left' : 'right'"
		:closable="false"
		:visible="showSettingsDrawer"
		width="360"
		:getContainer="() => wrapper"
		@close="$emit('toggleSettingsDrawer', false)"
	>

		<!-- Settings Drawer Close Button -->
		<a-button type="link" class="btn-close" @click="$emit('toggleSettingsDrawer', false)">
			<svg xmlns="http://www.w3.org/2000/svg" width="9" height="9" viewBox="0 0 9 9">
				<g id="close" transform="translate(0.75 0.75)">
					<path id="Path" d="M7.5,0,0,7.5" fill="none" stroke="#000" stroke-linecap="round" stroke-linejoin="round" stroke-miterlimit="10" stroke-width="1.5"/>
					<path id="Path-2" data-name="Path" d="M0,0,7.5,7.5" fill="none" stroke="#000" stroke-linecap="round" stroke-linejoin="round" stroke-miterlimit="10" stroke-width="1.5"/>
				</g>
			</svg>
		</a-button>
		<!-- / Settings Drawer Close Button -->
		
		<!-- Settings Drawer Content -->
		<div class="drawer-content">
			<h6>风格配置</h6>
			<p>调整你喜欢的样式.</p>
			<hr>
<!--			<div class="sidebar-color">-->
<!--				<h6>主题色</h6>-->
<!--				<a-radio-group v-model="sidebarColorModel" @change="$emit('updateSidebarColor', $event.target.value)" defaultValue="primary">-->
<!--					<a-radio-button value="primary" class="bg-primary"></a-radio-button>-->
<!--					<a-radio-button value="secondary" class="bg-secondary"></a-radio-button>-->
<!--					<a-radio-button value="success" class="bg-success"></a-radio-button>-->
<!--					<a-radio-button value="danger" class="bg-danger"></a-radio-button>-->
<!--					<a-radio-button value="warning" class="bg-warning"></a-radio-button>-->
<!--					<a-radio-button value="black" class="bg-dark"></a-radio-button>-->
<!--				</a-radio-group>-->
<!--			</div>-->
			<div class="sidenav-type">
				<h6>菜单显示风格</h6>
				<p>目前支持2种风格,其他风格后续更新</p>
				<a-radio-group button-style="solid" v-model="sidebarThemeModel" @change="$emit('updateSidebarTheme', $event.target.value)" defaultValue="primary">
					<a-radio-button value="light">透明</a-radio-button>
					<a-radio-button value="white">白底</a-radio-button>
				</a-radio-group>
			</div>
			<div class="navbar-fixed">
				<h6>导航锁定</h6>
				<a-switch default-checked v-model="navbarFixedModel" @change="$emit('toggleNavbarPosition', navbarFixedModel)" />
			</div>
<!--			<div class="download">-->
<!--				<a-button type="secondary" href="https://www.creative-tim.com/product/muse-vue-ant-design-dashboard" target="_blank" block>FREE DOWNLOAD</a-button>-->
<!--				<a-button type="dark" href="https://www.creative-tim.com/product/muse-vue-ant-design-dashboard-pro" target="_blank" block>BUY NOW</a-button>-->
<!--				<a-button type="secondary" href="https://demos.creative-tim.com/muse-vue-ant-design-dashboard-pro/documentation" block target="_blank">VIEW DOCUMENTATION</a-button>-->
<!--			</div>-->
<!--			<div class="github-stars">-->
<!--				<a-button-group>-->
<!--					<a-button size="small">-->
<!--						<svg xmlns="http://www.w3.org/2000/svg" width="14" height="14" viewBox="0 0 576 512"><path d="M528.1 171.5L382 150.2 316.7 17.8c-11.7-23.6-45.6-23.9-57.4 0L194 150.2 47.9 171.5c-26.2 3.8-36.7 36.1-17.7 54.6l105.7 103-25 145.5c-4.5 26.3 23.2 46 46.4 33.7L288 439.6l130.7 68.7c23.2 12.2 50.9-7.4 46.4-33.7l-25-145.5 105.7-103c19-18.5 8.5-50.8-17.7-54.6zM388.6 312.3l23.7 138.4L288 385.4l-124.3 65.3 23.7-138.4-100.6-98 139-20.2 62.2-126 62.2 126 139 20.2-100.6 98z"/></svg>-->
<!--						Star-->
<!--					</a-button>-->
<!--					<a-button size="small">-->
<!--						179-->
<!--					</a-button>-->
<!--				</a-button-group>-->
<!--			</div>-->
<!--			<div class="sharing">-->
<!--				<h6>Thank you for sharing!</h6>-->
<!--				<div class="share-links">-->
<!--					<a-button type="dark" size="small">-->
<!--						<svg width="15px" height="15px" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path d="M459.37 151.716c.325 4.548.325 9.097.325 13.645 0 138.72-105.583 298.558-298.558 298.558-59.452 0-114.68-17.219-161.137-47.106 8.447.974 16.568 1.299 25.34 1.299 49.055 0 94.213-16.568 130.274-44.832-46.132-.975-84.792-31.188-98.112-72.772 6.498.974 12.995 1.624 19.818 1.624 9.421 0 18.843-1.3 27.614-3.573-48.081-9.747-84.143-51.98-84.143-102.985v-1.299c13.969 7.797 30.214 12.67 47.431 13.319-28.264-18.843-46.781-51.005-46.781-87.391 0-19.492 5.197-37.36 14.294-52.954 51.655 63.675 129.3 105.258 216.365 109.807-1.624-7.797-2.599-15.918-2.599-24.04 0-57.828 46.782-104.934 104.934-104.934 30.213 0 57.502 12.67 76.67 33.137 23.715-4.548 46.456-13.32 66.599-25.34-7.798 24.366-24.366 44.833-46.132 57.827 21.117-2.273 41.584-8.122 60.426-16.243-14.292 20.791-32.161 39.308-52.628 54.253z"/></svg>-->
<!--						TWEET-->
<!--					</a-button>-->
<!--					<a-button type="dark" size="small">-->
<!--						<svg width="15px" height="15px" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 512 512"><path d="M503.691 189.836L327.687 37.851C312.281 24.546 288 35.347 288 56.015v80.053C127.371 137.907 0 170.1 0 322.326c0 61.441 39.581 122.309 83.333 154.132 13.653 9.931 33.111-2.533 28.077-18.631C66.066 312.814 132.917 274.316 288 272.085V360c0 20.7 24.3 31.453 39.687 18.164l176.004-152c11.071-9.562 11.086-26.753 0-36.328z"/></svg>-->
<!--						SHARE-->
<!--					</a-button>-->
<!--				</div>-->
<!--			</div>-->
		</div>
		<!-- / Settings Drawer Content -->

	</a-drawer>
	<!-- / Settings Drawer -->

</template>

<script>

	export default ({
		props: {
			// Settings drawer visiblility status.
			showSettingsDrawer: {
				type: Boolean,
				default: false,
			},
			
			// Main sidebar color.
			sidebarColor: {
				type: String,
				default: "primary",
			},
			
			// Main sidebar theme : light, white, dark.
			sidebarTheme: {
				type: String,
				default: "white",
			},

			// Header fixed status.
			navbarFixed: {
				type: Boolean,
				default: false,
			},

			// Drawer direction.
			rtl: {
				type: Boolean,
				default: false,
			},
		},
		data() {
			return {
				// The wrapper element to attach dropdowns to.
				wrapper: document.body,
				
				// Main sidebar color.
				sidebarColorModel: this.sidebarColor,
				
				// Main sidebar theme : light, white, dark.
				sidebarThemeModel: this.sidebarTheme,

				// Header fixed status.
				navbarFixedModel: this.navbarFixed,
			}
		},
		mounted: function(){
			// Set the wrapper to the proper element, layout wrapper.
			this.wrapper = document.getElementById('layout-dashboard') ;
		},
	})

</script>
