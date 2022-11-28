<template>
	
	<!-- Layout Footer -->
	<a-layout-footer>
		<a-row type="flex">
			<a-col :span="24" :md="12">

				<!-- Footer Copyright Notice -->
				<p class="copyright">
					© 2022, Made in China
					<svg width="20" height="20" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
						<path fill-rule="evenodd" clip-rule="evenodd" d="M3.17157 5.17157C4.73367 3.60948 7.26633 3.60948 8.82843 5.17157L10 6.34315L11.1716 5.17157C12.7337 3.60948 15.2663 3.60948 16.8284 5.17157C18.3905 6.73367 18.3905 9.26633 16.8284 10.8284L10 17.6569L3.17157 10.8284C1.60948 9.26633 1.60948 6.73367 3.17157 5.17157Z" fill="#111827"/>
					</svg>
					by <a href="http://folib.com">Fo Library</a> for a Artifact Store.
				</p>
				<!-- / Footer Copyright Notice -->

			</a-col>
			<a-col :span="24" :md="12" class="footer-control">

				<!-- Footer Navigation Menu -->
				<a-menu mode="horizontal">
					<a-menu-item><a href="https://bocloud.com.cn/" target="_blank">BoCloud</a></a-menu-item>
					<a-menu-item><a href="http://folib.com" target="_blank">关于我们</a></a-menu-item>
					<a-menu-item><a href="http://folib.com" target="_blank">博客</a></a-menu-item>
				</a-menu>
				<!-- / Footer Navigation Menu -->

			</a-col>
		</a-row>
	</a-layout-footer>
	<!-- / Layout Footer -->

</template>

<script>

	export default ({
		data() {
			return {
			}
		},
	})

</script>
