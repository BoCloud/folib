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
			<h6>{{ $t('Sidebars.StyleSettings') }}</h6>
			<p>{{ $t('Sidebars.AdjustStyleYouLike') }}</p>
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
				<h6>{{ $t('Sidebars.MenuDisplayStyle') }}</h6>
				<p>{{ $t('Sidebars.CurrentlySupportsStyles') }}</p>
				<a-radio-group button-style="solid" v-model="sidebarThemeModel" @change="$emit('updateSidebarTheme', $event.target.value)" defaultValue="primary">
					<a-radio-button value="light">{{ $t('Sidebars.Transparent') }}</a-radio-button>
					<a-radio-button value="white">{{ $t('Sidebars.WhiteBackground') }}</a-radio-button>
				</a-radio-group>
			</div>
			<div class="navbar-fixed">
				<h6>{{ $t('Sidebars.NavigationLock') }}</h6>
				<a-switch default-checked v-model="navbarFixedModel" @change="$emit('toggleNavbarPosition', navbarFixedModel)" />
			</div>
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
