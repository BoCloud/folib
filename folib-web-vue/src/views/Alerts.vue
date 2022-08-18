<!-- 
	This is the Alerts page, it uses the dashboard layout in: 
	"./layouts/Dashboard.vue" .
 -->

<template>
	<div>

		<a-row :gutter="24" type="flex" align="stretch">

			<!-- Default Alerts -->
			<a-col :span="24" :md="16" class="mb-24 mx-auto">
				<a-card :bordered="false" class="header-solid">
					<template #title>
						<h6>Alerts</h6>			
					</template>
    				<a-space direction="vertical" :size="16" style="width: 100%;">
						<a-alert message="Success Tips" type="success" show-icon />
						<a-alert message="Informational Notes" type="info" show-icon />
						<a-alert message="Warning" type="warning" show-icon />
						<a-alert message="Error" type="error" show-icon />
						<a-alert
							message="Success Tips"
							description="Detailed description and advices about successful copywriting."
							type="success"
							show-icon
						/>
						<a-alert
							message="Informational Notes"
							description="Additional description and informations about copywriting."
							type="info"
							show-icon
						/>
						<a-alert
							message="Warning"
							description="This is a warning notice about copywriting."
							type="warning"
							show-icon
						/>
						<a-alert
							message="Error"
							description="This is an error message about copywriting."
							type="error"
							show-icon
						/>
					</a-space>
				</a-card>
			</a-col>
			<!-- / Default Alerts -->
			
			<!-- Banners -->
			<a-col :span="24" :md="16" class="mb-24 mx-auto">
				<a-card :bordered="false" class="header-solid">
					<template #title>
						<h6>Banners</h6>			
					</template>
    				<a-space direction="vertical" :size="16" style="width: 100%;">
						<a-alert message="Success Tips" type="success" show-icon banner />
						<a-alert message="Informational Notes" type="info" show-icon banner />
						<a-alert message="Warning" type="warning" show-icon banner />
						<a-alert message="Error" type="error" show-icon banner />
						<a-alert
							message="Success Tips"
							description="Detailed description and advices about successful copywriting."
							type="success"
							show-icon
							banner
						/>
						<a-alert
							message="Informational Notes"
							description="Additional description and informations about copywriting."
							type="info"
							show-icon
							banner
						/>
						<a-alert
							message="Warning"
							description="This is a warning notice about copywriting."
							type="warning"
							show-icon
							banner
						/>
						<a-alert
							message="Error"
							description="This is an error message about copywriting."
							type="error"
							show-icon
							banner
						/>
					</a-space>
				</a-card>
			</a-col>
			<!-- / Banners -->
			
			<!-- Closable Alerts -->
			<a-col :span="24" :md="16" class="mb-24 mx-auto">
				<a-card :bordered="false" class="header-solid">
					<template #title>
						<h6>Closable</h6>			
					</template>
    				<a-space direction="vertical" :size="16" style="width: 100%;">
						<a-alert message="Success Tips" type="success" show-icon closable />
						<a-alert message="Informational Notes" type="info" show-icon closable />
						<a-alert message="Warning" type="warning" show-icon closable />
						<a-alert message="Error" type="error" show-icon closable />
						<a-alert
							message="Success Tips"
							description="Detailed description and advices about successful copywriting."
							type="success"
							show-icon
							closable
						/>
						<a-alert
							message="Informational Notes"
							description="Additional description and informations about copywriting."
							type="info"
							show-icon
							closable
						/>
						<a-alert
							message="Warning"
							description="This is a warning notice about copywriting."
							type="warning"
							show-icon
							closable
						/>
						<a-alert
							message="Error"
							description="This is an error message about copywriting."
							type="error"
							show-icon
							closable
						/>
					</a-space>
				</a-card>
			</a-col>
			<!-- / Closable Alerts -->
			
		</a-row>

		


	</div>
</template>

<script>

	export default ({
		data() {
			return {
			}
		},
	})

</script>

<style lang="scss">
</style>