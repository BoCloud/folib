<template>
	<a-card :bordered="false" class="header-solid storage-monitoring-data card-shadow"
		:bodyStyle="{ paddingTop: 0, paddingBottom: '0px' }">
		<template #title>
			<h6 class="font-semibold m-0">{{ $t('StorageMonitoring.StorageDetail') }}</h6>
		</template>
		<div class="mx-25 search" style="margin-top:-60px;margin-right:-15px !important;">
			<a-col :span="24" class="text-right">
				<a-select @change="storageChange" showSearch allowClear v-model="queryParams.storageId"
					v-if="!storageId" :placeholder="$t('StorageMonitoring.StorageQuery')" class="v-search">
					<a-select-option v-for="(storage, index) in storageList" :key="index" :label="storage.id"
						:value="storage.id">
						{{ storage.id }}
					</a-select-option>
				</a-select>
				<a-select @change="repositoryChange" showSearch allowClear v-model="queryParams.repositoryId"
					:placeholder="$t('StorageMonitoring.RepositoryQuery')" class="v-search">
					<a-select-option v-for="(repository, index) in repositoryList" :key="index" :label="repository.id"
						:value="repository.id">
						{{ repository.id }}
					</a-select-option>
				</a-select>
				<a-select @change="handheTableSearch" showSearch allowClear v-model="queryParams.repositorySubLayout"
					:placeholder="$t('StorageMonitoring.RepositoryLayoutQuery')" class="v-search">
					<a-select-option v-for="(layout, index) in layoutList" :key="index" :label="layout.key"
						:value="layout.value">
						{{ layout.key }}
					</a-select-option>
				</a-select>
				<a-select @change="handheTableSearch" allowClear v-model="queryParams.repositoryType"
					:placeholder="$t('StorageMonitoring.RepositoryTypeQuery')" class="v-search">
					<a-select-option v-for="(repositoryType, index) in repositoryTypeList" :key="index"
						:value="repositoryType.value">
						{{ getRepositoryType(repositoryType.value) }}
					</a-select-option>
				</a-select>
			</a-col>
		</div>
		<a-table rowKey="id" class="mt-20" :columns="i18nColumns"  :data-source="storageData" @change="handleChangeTable"
			:scroll="{ x: true,}" :loading="storageTableLoading"
			:pagination="{ pageSize: queryParams.limit, current: queryParams.page, total: queryParams.total, showLessItems: true }">
			<template slot="storageId" slot-scope="storageId, row">
				<span v-if="row.dataType === 4">{{ $t('StorageMonitoring.Total') }}</span>
				<span v-else-if="row.dataType === 2">{{ $t('StorageMonitoring.TrashCan') }}</span>
				<span v-else>{{ storageId }}</span>
			</template>
			<template slot="repositoryId" slot-scope="repositoryId, row">
				<span v-if="repositoryId">{{ row.repositoryId }}</span>
				<span v-else>N/A</span>
			</template>
			<template slot="repositoryLayout" slot-scope="repositoryLayout, row">
				<span v-if="repositoryLayout">
					<a-avatar :size="48" shape="square"
						style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
						:src="'images/folib/' + getLayoutType({ layout: row.repositoryLayout, subLayout: row.repositorySubLayout }) + '.svg'" />
					<span class="ml-10">{{ showLayout(row.repositorySubLayout) }}</span>
				</span>
				<span v-else>N/A</span>
			</template>
			<template slot="repositoryType" slot-scope="repositoryType">
				<span v-if="repositoryType === 'hosted'"> {{ $t('StorageMonitoring.HostedRepository') }}</span>
				<span v-else-if="repositoryType === 'proxy'">{{ $t('StorageMonitoring.ProxyRepository') }}</span>
				<span v-else>N/A</span>
			</template>
			<template slot="usedStorageQuotaSizePercentage" slot-scope="usedStorageQuotaSizePercentage, row">
				<span v-if="row.dataType === 4 || row.dataType === 2">N/A</span>
				<span v-else-if="usedStorageQuotaSizePercentage >= 90">
					<a-tag color="red">{{ usedStorageQuotaSizePercentage.toFixed(2) + '%' }}</a-tag>
				</span>
				<span v-else-if="usedStorageQuotaSizePercentage >= 80">
					<a-tag color="orange">{{ usedStorageQuotaSizePercentage.toFixed(2) + '%' }}</a-tag>
				</span>
				<span v-else>
					{{ usedStorageQuotaSizePercentage.toFixed(2) + '%' }}
				</span>
			</template>
			<template slot="usedStorageDeviceSizePercentage" slot-scope="usedStorageDeviceSizePercentage, row">
				<span v-if="row.dataType === 4 || row.dataType === 2">N/A</span>
				<span v-else-if="usedStorageDeviceSizePercentage >= 90">
					<a-tag color="red">{{ usedStorageDeviceSizePercentage.toFixed(2) + '%' }}</a-tag>
				</span>
				<span v-else-if="usedStorageDeviceSizePercentage >= 80">
					<a-tag color="orange">{{ usedStorageDeviceSizePercentage.toFixed(2) + '%' }}</a-tag>
				</span>
				<span v-else>
					{{ usedStorageDeviceSizePercentage.toFixed(2) + '%' }}
				</span>
			</template>
			<template slot="filesSize" slot-scope="filesSize">
				<span> {{ fileSizeConver(filesSize) }}</span>
			</template>
			<template slot="artifactsSize" slot-scope="artifactsSize">
				<span>{{ fileSizeConver(artifactsSize) }}</span>
			</template>
		</a-table>
	</a-card>

</template>

<script>

import {
	getStorages,
	getLibrary,
} from "@/api/folib"
import {
	getLayouts,
} from "@/api/artifact"
import {
	getStorageMonitoringList,
	getStorageMonitoringPage,
} from "@/api/storageMonitoring";
import { getLayoutType, fileSizeConver } from "@/utils/layoutUtil"


export default ({
	props: {
		storageId: {
			type: String,
			default: undefined,
		},
	},
	components: {

	},
	watch: {
		storageId: {
			handler(newVal) {
				if (this.storageId) {
					this.queryParams.storageId = this.storageId
					this.storageChange()
				}
			},
			deep: true,
		},
	},
	data() {
		return {
			columns: [
				{
					title: "存储空间",
					i18nKey: 'StorageMonitoring.Storage',
					dataIndex: "storageId",
					scopedSlots: { customRender: "storageId" },
					width: 130,
				},
				{
					title: "所属仓库",
					i18nKey: 'StorageMonitoring.Repository',
					dataIndex: "repositoryId",
					scopedSlots: { customRender: "repositoryId" },
					width: 160,
				},
				{
					title: "包类型",
					i18nKey: 'StorageMonitoring.RepositoryLayout',
					dataIndex: "repositoryLayout",
					scopedSlots: { customRender: "repositoryLayout" },
					width: 200,
				},
				{
					title: "仓库类型",
					i18nKey: 'StorageMonitoring.RepositoryType',
					dataIndex: "repositoryType",
					scopedSlots: { customRender: "repositoryType" },
					width: 150,
				},
				{
					title: "配额占比",
					i18nKey: 'StorageMonitoring.UsedStorageQuotaSizePercentage',
					dataIndex: "usedStorageQuotaSizePercentage",
					sorter: true,
					scopedSlots: { customRender: "usedStorageQuotaSizePercentage" },
					width: 150,
				},
				{
					title: "磁盘占比",
					i18nKey: 'StorageMonitoring.UsedStorageSizePercentage',
					dataIndex: "usedStorageDeviceSizePercentage",
					sorter: true,
					scopedSlots: { customRender: "usedStorageDeviceSizePercentage" },
					width: 150,
				},
				{
					title: "文件大小",
					i18nKey: 'StorageMonitoring.FilesSize',
					dataIndex: "filesSize",
					sorter: true,
					scopedSlots: { customRender: "filesSize" },
					width: 150,
				},
				{
					title: "文件数量",
					i18nKey: 'StorageMonitoring.FilesCount',
					dataIndex: "filesCount",
					sorter: true,
					scopedSlots: { customRender: "filesCount" },
					width: 150,
				},
				{
					title: "制品大小",
					i18nKey: 'StorageMonitoring.ArtifactsSize',
					dataIndex: "artifactsSize",
					sorter: true,
					scopedSlots: { customRender: "artifactsSize" },
					width: 150,
				},
				{
					title: "制品数量",
					i18nKey: 'StorageMonitoring.ArtifactsCount',
					dataIndex: "artifactsCount",
					sorter: true,
					scopedSlots: { customRender: "artifactsCount" },
					width: 150,
				},
				{
					title: "文件夹数量",
					i18nKey: 'StorageMonitoring.FoldersCount',
					dataIndex: "foldersCount",
					sorter: true,
					scopedSlots: { customRender: "foldersCount" },
					width: 150,
				},
				{
					title: "条目数量",
					i18nKey: 'StorageMonitoring.ItemsCount',
					dataIndex: "itemsCount",
					sorter: true,
					scopedSlots: { customRender: "itemsCount" },
					width: 150,
				},
			],
			storageData: [],
			storageList: [],
			repositoryList: [],
			layoutList: [],
			repositoryTypeList: [
				{
					label: this.$t('StorageMonitoring.HostedRepository'),
					value: 'hosted',
				},
				{
					label: this.$t('StorageMonitoring.ProxyRepository'),
					value: 'proxy',
				}
			],
			storageTableLoading: false,
			queryParams: {
				limit: 10,
				page: 1,
				total: 0,
				isLatest: 1,
				storageId: undefined,
				repositoryId: undefined,
				repositorySubLayout: undefined,
				repositoryType: undefined,
				dataTypes: [1, 2, 4],
				sortField: undefined,
				sortOrder: undefined,
			},
		}
	},
	computed: {
		i18nColumns() {
			return this.columns.map(column => {
				if (column.i18nKey) {
					column.title = this.$t(column.i18nKey);
				}
				return column;
			})
		},
	},
	created() {
		this.init()
	},
	methods: {
		fileSizeConver(size) {
			if (size >= 0) {
				return fileSizeConver(size)
			}
		},
		init() {
			if (this.storageId) {
				this.queryParams.storageId = this.storageId
				this.storageChange()
			}
			this.getLayouts()
			this.getStorages()
			this.getStorageMonitoringData()
		},
		getStorageMonitoringData() {
			this.storageTableLoading = true
			getStorageMonitoringPage(this.queryParams).then((res) => {
				this.storageData = res.data.rows
				this.queryParams.total = res.data.total
			}).finally(() => {
				this.storageTableLoading = false
			})
		},
		handheTableSearch() {
			this.queryParams.page = 1
			this.getStorageMonitoringData()
		},
		handleChangeTable(pagination, filters, sorter) {
			this.queryParams.sortField = null
			this.queryParams.sortOrder = null
			if (pagination) {
				this.queryParams.page = pagination.current
			}
			if (sorter) {
				this.queryParams.sortField = sorter.field
				if (sorter.order) {
					this.queryParams.sortOrder = 'asc'
					if (sorter.order.indexOf('desc') !== -1) {
						this.queryParams.sortOrder = 'desc'
					}
				}
			}
			this.getStorageMonitoringData()
		},
		getLayoutType(item) {
			return getLayoutType(item)
		},
		getStorages() {
			getStorages().then(response => {
				this.storageList = response.storages
			})
		},
		getLibrary() {
			this.repositoryList = []
			getLibrary(this.queryParams.storageId).then(response => {
				this.repositoryList = response.repositories
			})
		},
		storageChange() {
			this.getLibrary()
			this.handheTableSearch()
		},
		repositoryChange() {
			this.handheTableSearch()
		},
		getLayouts() {
			getLayouts().then(response => {
				this.layoutList = response
			})
		},
		showLayout(subLayout) {
			let layout = subLayout
			if (this.layoutList) {
				let layouts = this.layoutList.filter((layout) => {
					return layout.value == subLayout
				})
				if (layouts && layouts.length > 0) {
					layout = layouts[0].key
				}
			}
			return layout
		},
		getRepositoryType(repositoryType) {
			let type = repositoryType
			if (type) {
				if (type === 'hosted') {
					type = this.$t('StorageMonitoring.HostedRepository')
				} else if (type === 'proxy') {
					type = this.$t('StorageMonitoring.ProxyRepository')
				}
			}
			return type
		},
	}
})

</script>

<style lang="scss" scoped>
.storage-monitoring-data {
	.search {
		height: 50px;
	}

	.mx-25 .ant-row-flex {
		flex-wrap: wrap;
	}

	.v-search {
		max-width: 200px;
		width: 170px;
		min-width: 150px;
		margin-left: 5px;
		margin-bottom: 8px;
	}
}
</style>