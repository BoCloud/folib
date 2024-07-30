<template>

	<a-card :bordered="false" class="header-solid storage-monitoring-overview" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }">
		<template #title>
			<h6 class="font-semibold m-0">
				{{$t('StorageMonitoring.StorageOverview')}}
				<span v-if="!latestData">
					<a-button type="link" size="small" @click="doUpdateStorageMonitoringData">
						<a-icon type="redo" />
						立即刷新
					</a-button>
				</span>
			</h6>
		</template>
		<a-row :gutter="[24, 24]">
			<a-col :span="24" v-for="(item, index) in latestData" :key="index">
				<a-card :bordered="false" class="disk-info">
					<a-col :span="8">
						<a-descriptions title="" :column="1">
							<a-descriptions-item :label="$t('StorageMonitoring.FilesSize')">
								{{ fileSizeConver(item.filesSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.FilesCount')">
								{{ item.filesCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.ArtifactsSize')">
								{{ fileSizeConver(item.artifactsSize) }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.ArtifactsCount')">
								{{ item.artifactsCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.FoldersCount')">
								{{ item.foldersCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.ItemsCount')">
								{{ item.itemsCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.StorageCount')">
								{{ item.storageCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.RepositoryCount')">
								{{ item.repositoryCount }}
							</a-descriptions-item>
							<a-descriptions-item :label="$t('StorageMonitoring.DataUpdateTime')">
								<span>{{ item.createTime }}</span>
								<a-button type="link" size="small" @click="doUpdateStorageMonitoringData">
									<a-icon type="redo" />
									立即刷新
								</a-button>
							</a-descriptions-item>
						</a-descriptions>
					</a-col>
					<a-col :span="16">
						<ChartMixed :height="260" :chartData="mixedChartData"/>
						<div class="chart-mixed-tips">{{ $t('StorageMonitoring.LastTwentyUpdates') }} (GB)</div>
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
		updateStorageMonitoringData
	} from "@/api/storageMonitoring";

	export default ({
		components: {
			ChartBar,
			ChartPie,
			ChartMixed,
			ChartDoughnut,
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
					dataType: 4,
					sortField: 'createTime',
					sortOrder: 'DESC',
				},
			}
		},
		created() {
			this.init()
		},
		methods: {
			init() {
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
			getStorageMonitorings() {
				this.latestData = []
				getStorageMonitoringList({isLatest: true, dataType: 4}).then(response => {
					this.latestData = response
				})
			},
			getStorageMonitoringData() {
				getStorageMonitoringPage(this.queryParams).then((res) => {
					let data = res.data.rows
					if (data) {
						this.storageData = [...data].reverse()
					} else {
						this.storageData = []
					}
					this.mixedChartData.labels = []
					this.mixedChartData.datasets[0].data = []
					for (let index in this.storageData) {
						let storage = this.storageData[index]
						this.mixedChartData.labels.push(storage.createDate)
						this.mixedChartData.datasets[0].data.push(this.fileSizeConverUnit(storage.filesSize))
					}
					this.queryParams.total = res.data.total
				}).finally(() => {
				})
			},
			doUpdateStorageMonitoringData() {
				updateStorageMonitoringData().then((res) => {
					
				}).finally(() => {
				})
				setTimeout(() => {
					this.message('success', this.$t('StorageMonitoring.RefreshData'))
				}, 200)
			}
		}
	})

</script>

<style lang="scss" scoped>
.storage-monitoring-overview {
	.disk-info {
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