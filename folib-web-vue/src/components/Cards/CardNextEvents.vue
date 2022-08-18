<template>

	<!-- Conversations Card -->
	<a-card :bordered="false" class="card-next-events header-solid" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
		<template #title>
			<h6 class="font-semibold m-0">Next Events</h6>
		</template>
		<a-list
			class="next-events-list"
			item-layout="horizontal"
			:split="false"
			:data-source="data"
		>
			<a-list-item slot="renderItem" slot-scope="item">
				<a-list-item-meta
					:title="item.title"
					:description="item.code"
				>
					<a-avatar
					slot="avatar"
					:size="48"
					:style="{background: item.iconBgColor}"
					class="avatar-icon"
					shape="square">
						<a-icon :type="item.icon" :class="item.iconClass" theme="filled" />
					</a-avatar>
				</a-list-item-meta>
			</a-list-item>
		</a-list>
	</a-card>
	<!-- / Conversations Card -->

</template>

<script>

	export default ({
		props: {
			data: {
				type: Array,
				default: () => [],
			},
		},
		data() {
			return {
			}
		},
	})

</script>

<style scoped>
	.ni {
		font-size: 20px;
	}
</style>