<template>

	<a-card :bordered="false" class="header-solid storage-monitoring-overview" v-if="latestData" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }">
		<template #title>
			<h6 class="font-semibold m-0">{{$t('Storage.StorageOverview')}}</h6>
		</template>
		<a-row :gutter="[24, 24]">
			<a-col :span="24" v-for="(item, index) in latestData" :key="index">
				<a-card :bordered="false" class="header-solid storage-device-info">
					<a-col :span="8">
						<a-descriptions title="" :column="1">
							<a-descriptions-item :label="$t('Storage.RepositoryCount')">
								{{ item.repositoryCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.FilesSize')">
								{{ fileSizeConver(item.filesSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.FilesCount')">
								{{ item.filesCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.ArtifactsSize')">
								{{ fileSizeConver(item.artifactsSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.ArtifactsCount')">
								{{ item.artifactsCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.FoldersCount')">
								{{ item.foldersCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.ItemsCount')">
								{{ item.itemsCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.StorageDeviceType')">
								{{ item.storageDeviceType }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.StorageQuotaSize')">
								{{ fileSizeConver(item.storageQuotaSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('Storage.DataUpdateTime')">
								<span>{{ item.createTime }}</span>
							</a-descriptions-item>
						</a-descriptions>
					</a-col>
					<a-col :span="16">
						<ChartMixed :height="260" :chartData="mixedChartData"/>
						<div class="chart-mixed-tips">{{ $t('Storage.LastTwentyUpdates') }} (GB)</div>
					</a-col>
				</a-card>
			</a-col>
		</a-row>
	</a-card>

</template>

<script>

	import {
			getLayoutType,
			getFileType,
			fileSizeConver,
			fileSizeConverUnit,
			formateDate,
	} from "@/utils/layoutUtil"
	import ChartBar from '@/components/Charts/ChartBar';
	import ChartPie from '@/components/Charts/ChartPie';
	import ChartMixed from '@/components/Charts/ChartMixed';
	import ChartDoughnut from '@/components/Charts/ChartDoughnut';

	import {
		getStorageMonitoringList,
		getStorageMonitoringPage,
	} from "@/api/storageMonitoring";

	export default ({
		props: {
			storageId: {
				type: String,
				default: undefined,
			},
		},
		components: {
			ChartBar,
			ChartPie,
			ChartMixed,
			ChartDoughnut,
		},
		watch: {
			storageId: {
				handler(newVal) {
					if (this.storageId) {
						this.init()
					}
				},
				deep: true,
			},
		},
		data() {
			return {
				mixedChartData: {
					labels: [],
					datasets: [
						{
							type: "bar",
							label: "",
							weight: 5,
							tension: 0.4,
							borderWidth: 0,
							pointBackgroundColor: "#1890FF",
							borderColor: "#1890FF",
							backgroundColor: '#1890FF',
							borderRadius: 4,
							borderSkipped: false,
							data: [],
							maxBarThickness: 10,
						},
					],
				},
				latestData: [
					
				],
				queryParams: {
					limit: 20,
					page: 1,
					total: 0,
					storageId: null,
					repositoryId: null,
					repositoryLayout: null,
					repositoryType: null,
					dataType: 3,
					sortField: 'createTime',
					sortOrder: 'DESC',
				},
			}
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
			fileSizeConverUnit(size, unit) {
				if (!unit) {
					unit = 'GB'
				}
				if (size >= 0) {
					return fileSizeConverUnit(size, unit)
				}
			},
			init() {
				if (this.storageId) {
					this.queryParams.storageId = this.storageId
				}
				this.getStorageMonitorings()
				this.getStorageMonitoringData()
			},
			message(type, message) {
				if (!message) {
					message = this.$t('Storage.OperationSuccessful')
				}
				this.$notification[type]({
					message: message,
					description: "",
				})
			},
			getStorageMonitorings() {
				getStorageMonitoringList({storageId: this.storageId, isLatest: true, dataType: 3}).then(response => {
					this.latestData = response
				})
			},
			getStorageMonitoringData() {
				getStorageMonitoringPage(this.queryParams).then((res) => {
					let data = res.data.rows
					let storageData = []
					if (data) {
						storageData = [...data].reverse()
					}
					this.mixedChartData.labels = []
					this.mixedChartData.datasets[0].data = []
					for (let index in storageData) {
						let storage = storageData[index]
						this.mixedChartData.labels.push(storage.createDate)
						this.mixedChartData.datasets[0].data.push(this.fileSizeConverUnit(storage.filesSize))
					}
					this.queryParams.total = res.data.total
				}).finally(() => {
				})
			},

		}
	})

</script>

<style lang="scss" scoped>
.storage-monitoring-overview {
	.storage-device-info {
		box-shadow: none;
	}
	.storage-info {
		box-shadow: none;
		border: none;
	}
	.storage-info img {
		width: 32px;
		height: 32px;
	}
	.chart-mixed-tips {
		text-align: center;
		font-weight: bold;
  	color: black;
	}
}
</style>