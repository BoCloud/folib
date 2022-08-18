<template>

	<!-- Conversations Card -->
	<a-card :bordered="false" class="card-categories header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: '12px' }">
		<template #title>
			<h6 class="font-semibold m-0">Categories</h6>
		</template>
		<a-list
			class="categories-list"
			item-layout="horizontal"
			:split="false"
			:data-source="data"
		>
			<a-list-item slot="renderItem" slot-scope="item">
				<a-button slot="actions" type="link">
					<a-icon type="right" class="text-dark" />
				</a-button>
				<a-list-item-meta
					:title="item.title"
				>
					<span slot="description" class="text-sm" v-html="item.subtitle"></span>
					<a-avatar
					slot="avatar"
					:size="32"
					style="border-radius: 12px;"
					shape="square"
					class="bg-gradient-dark">
						<a-icon :type="item.icon" :theme="item.iconTheme ? item.iconTheme : 'filled'" :twoToneColor="item.iconTwoToneColor ? item.iconTwoToneColor : '#141414'" />
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

	.categories-list::v-deep .anticon {
		font-size: 14px;
	}

	.categories-list::v-deep .anticon svg {
		vertical-align: 1px;
	}

</style>