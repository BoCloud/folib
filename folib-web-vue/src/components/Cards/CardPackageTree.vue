<template>

	<!-- Platform Settings Card -->
	<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: 0 }">
		<template #title>
			<h6 class="font-semibold m-0">包列表</h6>
		</template>
    <a-tree
        v-model:expandedKeys="expandedKeys"
        v-model:selectedKeys="selectedKeys"
        :load-data="onLoadData"
        :tree-data="treeData"
    />
	</a-card>
	<!-- / Platform Settings Card -->

</template>

<script>

	export default ({
		data() {
			return {
        expandedKeys,
        selectedKeys,
        treeData,
        onLoadData,
			}
		},
	})

</script>
