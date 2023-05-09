<template>

	<!-- Settings Drawer -->
	<a-drawer
		class="settings-drawer"
		:class="[ rtl ? 'settings-drawer-rtl' : '' ]"
		:placement="rtl ? 'left' : 'right'"
		:closable="false"
		:visible="showUploadProcessDrawer"
		width="25%"
		:getContainer="() => wrapper"
		@close="$emit('uploadProcessDrawer', false)"
	>

		<!-- Settings Drawer Close Button -->
		<a-button type="link" class="btn-close" @click="$emit('uploadProcessDrawer', false)">
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
			<h6>上传进度</h6>
			<hr>
			<a-card :bordered="false" class="header-solid h-full upload-process-card" :bodyStyle="{paddingTop: 0, paddingBottom: '12px' }">
				<template #title>
					<h6 class="font-semibold m-0">文件列表 
						<a-tooltip class="process-reset-tip">
							<template slot="title">清空</template>
							<div class="">
								<a class="ml-10 process-reset" @click="uploadProcessRemove('')"><a-icon type="rest" /></a>
							</div>
						</a-tooltip>
					</h6>
				</template>
				<a-list
					class="categories-list"
					item-layout="horizontal"
					:split="false"
					:data-source="uploadProcessList"
					:pagination="uploadProcessList.length === 0 ? false : { pageSize: 6, total: uploadProcessList.length, showLessItems: true }"
				>
					<a-list-item slot="renderItem" slot-scope="item">
						<a-list-item-meta
							:title="item.alias"
						>
							<span slot="description" class="text-sm">
								<p v-if="item.comment && item.comment.length > 0"><span v-if="!item.dictKey.includes('zip_')">失败原因：</span> {{ item.comment }}</p>
								<a-progress :percent="item.dictValue" :status="(item.comment && item.comment.length > 0 && !item.dictKey.includes('zip_'))?'exception':item.dictValue<100?'active':'success'" />

								<div class="process-action">
									<a-button type="link" size="small" @click="uploadProcessRemove(item.dictKey)">
										<svg width="16" height="16" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
										<path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd"
											d="M9 2C8.62123 2 8.27497 2.214 8.10557 2.55279L7.38197 4H4C3.44772 4 3 4.44772 3 5C3 5.55228 3.44772 6 4 6L4 16C4 17.1046 4.89543 18 6 18H14C15.1046 18 16 17.1046 16 16V6C16.5523 6 17 5.55228 17 5C17 4.44772 16.5523 4 16 4H12.618L11.8944 2.55279C11.725 2.214 11.3788 2 11 2H9ZM7 8C7 7.44772 7.44772 7 8 7C8.55228 7 9 7.44772 9 8V14C9 14.5523 8.55228 15 8 15C7.44772 15 7 14.5523 7 14V8ZM12 7C11.4477 7 11 7.44772 11 8V14C11 14.5523 11.4477 15 12 15C12.5523 15 13 14.5523 13 14V8C13 7.44772 12.5523 7 12 7Z"
											fill="#111827" />
										</svg>
										<span class="text-danger">DELETE</span>
									</a-button>
								</div>
							</span>
						</a-list-item-meta>
					</a-list-item>
				</a-list>
			</a-card>
		</div>
		<!-- / Settings Drawer Content -->

	</a-drawer>
	<!-- / Settings Drawer -->

</template>

<script>
	import {
	  queryArtifactUploadProcess,
	  deleteArtifactUploadProcess,
	} from "@/api/artifact"
	export default ({
		inject: ["reload"],
		props: {
			// Settings drawer visiblility status.
			showUploadProcessDrawer: {
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
				default: "light",
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

				uploadProcessList: [],

				notFinishUploadList: [],
			}
		},
		mounted: function(){
			// Set the wrapper to the proper element, layout wrapper.
			this.wrapper = document.getElementById('layout-dashboard')
		},
		created: function() {
		},
		watch: {
			showUploadProcessDrawer: function (newval, oldVal) {
				if (newval) {
					this.queryAllProcess()
				}
			},
		},
		methods: {
			interval() {
				const intervalId = setInterval(() => {
					if (this.incompleted()) {
						this.notFinishUploadList.forEach(element => {
							this.getProgressRate(element)
						})
					} else {
						clearInterval(intervalId)
					}
				}, 300);
			},
			incompleted (){
				this.notFinishUploadList = this.uploadProcessList.filter(item => item.dictValue < 100 && (!item.comment || item.comment.length <1))
				return this.notFinishUploadList.length > 0
			},
			//获取进度
			getProgressRate(element) {
				queryArtifactUploadProcess(element.dictKey).then((res) => {
					if (res && res.length > 0) {
						let data = res[0];
						element.dictValue = new Number(data.dictValue)
						element.comment = data.comment
					}
				})
			},
			queryAllProcess () {
				queryArtifactUploadProcess('').then((res) => {
					this.uploadProcessList = res
					if (this.uploadProcessList) {
						this.uploadProcessList.forEach(item => {
							item.dictValue = new Number(item.dictValue)
						})
						this.interval()
					}
				})
			},
			uploadProcessRemove(uuid) {
				deleteArtifactUploadProcess(uuid).then(() => {
					this.queryAllProcess()
				})
			}
		}
	})

</script>

<style lang="scss" scoped>
 .upload-process-card{
	min-height: 90vh;
 }

 .process-action button{
	padding: unset;
 }

 .process-reset-tip{
	display: inline-block;
 }

 .process-reset{
	color: red;
 }
</style>
