<!-- 
	This is the dashboard's rtl layout, used in rtl page.
 -->

<template>
	<div>

		<!-- Dashboard Layout -->
		<a-layout class="layout-dashboard layout-dashboard-rtl" id="layout-dashboard" :class="[navbarFixed ? 'navbar-fixed' : '', ! sidebarCollapsed ? 'has-sidebar' : '', layoutClass, sidebarMinimized ? 'sidebar-minimized' : '' ]">
			
			<!-- Settings Drawer -->
			<DashboardSettingsDrawer
				:showSettingsDrawer="showSettingsDrawer"
				:navbarFixed="navbarFixed"
				:sidebarTheme="sidebarTheme"
				:rtl="true"
				@toggleSettingsDrawer="toggleSettingsDrawer"
				@toggleNavbarPosition="toggleNavbarPosition"
				@updateSidebarTheme="updateSidebarTheme"
				@updateSidebarColor="updateSidebarColor"
			></DashboardSettingsDrawer>
			<!-- / Settings Drawer -->
			
			<!-- Main Sidebar -->
			<DashboardSidebar
				:sidebarCollapsed="sidebarCollapsed"
				:sidebarColor="sidebarColor"
				:sidebarTheme="sidebarTheme"
				@toggleSidebar="toggleSidebar"
			></DashboardSidebar>
			<!-- / Main Sidebar -->

			<!-- Layout Content -->
			<a-layout>

				<!-- Layout Header's Conditionally Fixed Wrapper -->
				<DashboardHeader
					:sidebarCollapsed="sidebarCollapsed"
					:navbarFixed="navbarFixed"
					@toggleSettingsDrawer="toggleSettingsDrawer"
					@toggleSidebar="toggleSidebar"
					@minimizeSidebar="minimizeSidebar"
				></DashboardHeader>
				<!-- / Layout Header's Conditionally Fixed Wrapper -->

				<!-- Page Content -->
				<a-layout-content>
					<router-view />
				</a-layout-content>
				<!-- / Page Content -->

				<!-- Layout Footer -->
				<DashboardFooter></DashboardFooter>
				<!-- / Layout Footer -->

				<!-- Sidebar Overlay -->
				<div class="sidebar-overlay" @click="sidebarCollapsed = true" v-show="! sidebarCollapsed"></div>
				<!-- / Sidebar Overlay -->
					
				<!-- Floating Action Button For Toggling Settings Drawer -->
				<a-button class="fab" shape="circle" @click="showSettingsDrawer = true">
					<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
						<path fill-rule="evenodd" clip-rule="evenodd" d="M11.4892 3.17094C11.1102 1.60969 8.8898 1.60969 8.51078 3.17094C8.26594 4.17949 7.11045 4.65811 6.22416 4.11809C4.85218 3.28212 3.28212 4.85218 4.11809 6.22416C4.65811 7.11045 4.17949 8.26593 3.17094 8.51078C1.60969 8.8898 1.60969 11.1102 3.17094 11.4892C4.17949 11.7341 4.65811 12.8896 4.11809 13.7758C3.28212 15.1478 4.85218 16.7179 6.22417 15.8819C7.11045 15.3419 8.26594 15.8205 8.51078 16.8291C8.8898 18.3903 11.1102 18.3903 11.4892 16.8291C11.7341 15.8205 12.8896 15.3419 13.7758 15.8819C15.1478 16.7179 16.7179 15.1478 15.8819 13.7758C15.3419 12.8896 15.8205 11.7341 16.8291 11.4892C18.3903 11.1102 18.3903 8.8898 16.8291 8.51078C15.8205 8.26593 15.3419 7.11045 15.8819 6.22416C16.7179 4.85218 15.1478 3.28212 13.7758 4.11809C12.8896 4.65811 11.7341 4.17949 11.4892 3.17094ZM10 13C11.6569 13 13 11.6569 13 10C13 8.34315 11.6569 7 10 7C8.34315 7 7 8.34315 7 10C7 11.6569 8.34315 13 10 13Z" fill="#111827"/>
					</svg>
				</a-button>
				<!-- / Floating Action Button For Toggling Settings Drawer -->

			</a-layout>
			<!-- / Layout Content -->

		</a-layout>
		<!-- / Dashboard Layout -->

	</div>
</template>

<script>

	import DashboardSidebar from '../components/Sidebars/DashboardSidebar' ;
	import DashboardHeader from '../components/Headers/DashboardHeader' ;
	import DashboardFooter from '../components/Footers/DashboardFooter' ;
	import DashboardSettingsDrawer from '../components/Sidebars/DashboardSettingsDrawer' ;

	export default ({
		components: {
			DashboardSidebar,
			DashboardHeader,
			DashboardFooter,
			DashboardSettingsDrawer,
		},
		data() {
			return {
				
				// Sidebar collapsed status.
				sidebarCollapsed: false,

				// Sidebar minimized status.
				sidebarMinimized: false,
				
				// Main sidebar color.
				sidebarColor: "primary",
				
				// Main sidebar theme : light, white, dark.
				sidebarTheme: "light",

				// Header fixed status.
				navbarFixed: false,

				// Settings drawer visiblility status.
				showSettingsDrawer: false,
				
			}
		},
		methods: {

			// Toggle sidebar's collapsed status.
			toggleSidebar( value ) {
				this.sidebarCollapsed = value ;
			},
			
			// Toggle sidebar's minimized status.
			minimizeSidebar( value ) {
				this.sidebarMinimized = ! this.sidebarMinimized ;
			},
			
			// Toggle ettings drawer's visiblility status.
			toggleSettingsDrawer( value ) {
				this.showSettingsDrawer = value ;
			},
			
			// Toggle navbar's fixed status.
			toggleNavbarPosition( value ) {
				this.navbarFixed = value ;
			},
			
			// Change sidebar's theme.
			updateSidebarTheme( value ) {
				this.sidebarTheme = value ;
			},
			
			// Change sidebar's color.
			updateSidebarColor( value ) {
				this.sidebarColor = value ;
			},

		},
		computed: {
			// Sets layout's element's class based on route's meta data.
			layoutClass() {
				return this.$route.meta.layoutClass ;
			}
		},
	})

</script>
