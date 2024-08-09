<template>

	<a-row :gutter="[24, 24]" class="storage-monitoring-overview">
		<a-col :span="24">
			<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{ paddingTop: 0, paddingBottom: '16px' }">
				<template #title>
					<h6 class="font-semibold m-0">
						{{ $t('StorageMonitoring.StorageOverview') }}
						<span v-if="!storageId">
							<a-button type="link" size="small" @click="doUpdateStorageMonitoringData">
								<a-icon type="redo" />
								{{ $t('StorageMonitoring.RefreshImmediately')}}
							</a-button>
						</span>
					</h6>
				</template>
				<a-list class="conversations-list" :grid="{ gutter: 24, column: 4 }" item-layout="horizontal" :split="false"
					:data-source="latestData">
					<a-list-item slot="renderItem" slot-scope="item">
						<a-list-item-meta :title="$t(item.title)" :description="item.code">
							<a-avatar slot="avatar" :size="48" :style="{ backgroundColor: item.color }" shape="square"
								:icon="item.avatar ? item.avatar : ''" :src="item.src ? item.src : ''" />
						</a-list-item-meta>
					</a-list-item>
				</a-list>
			</a-card>
		</a-col>

		<a-col :span="24">
			<a-row :gutter="[24, 24]" type="flex" align="stretch">
				<a-col :span="24" :lg="10" class="mb-24">
					<CardChartMixed :data="mixedChartData" :storageMonitoringData="storageMonitoringData"
					:storageDeviceCount="storageDeviceCount" />
					<!-- <CardBarChart :data="barChartData" :storageMonitoringData="storageMonitoringData"
						:storageDeviceCount="storageDeviceCount" /> -->
				</a-col>
				<a-col :span="24" :lg="14" class="mb-24">
					<CardLineChart :data="lineChartData" :storageMonitoringData="storageMonitoringData" />
				</a-col>
			</a-row>
		</a-col>
	</a-row>

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
import ChartLine from '@/components/Charts/ChartLine';
import CardBarChart from './CardBarChart';
import CardChartMixed from './CardChartMixed';
import CardLineChart from './CardLineChart';
import WidgetCounter from '@/components/Widgets/WidgetCounter';

import {
	getStorageMonitoringList,
	getStorageMonitoringPage,
	updateStorageMonitoringData
} from "@/api/storageMonitoring";

export default ({
	props: {
		storageId: {
			type: String,
			default: ''
		},
	},
	components: {
		ChartBar,
		ChartPie,
		ChartMixed,
		ChartDoughnut,
		WidgetCounter,
		ChartLine,
		CardBarChart,
		CardChartMixed,
		CardLineChart
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
			storageDeviceCount: '-',
			lineChartData: {
				labels: [],
				datasets: [{
					label: "Mobile apps",
					tension: 0.4,
					borderWidth: 0,
					pointRadius: 0,
					borderColor: "#1890FF",
					borderWidth: 3,
					data: [],
					maxBarThickness: 6

				},
				{
					label: 'Websites',
					tension: 0.4,
					borderWidth: 0,
					pointRadius: 0,
					borderColor: "#B37FEB",
					borderWidth: 3,
					data: [],
					maxBarThickness: 6

				}],
			},
			mixedChartData: {
				labels: [],
				datasets: [
					{
						type: "bar",
						label: "",
						weight: 5,
						tension: 0.4,
						borderWidth: 0,
						pointBackgroundColor: "#141414",
						borderColor: "#141414",
						backgroundColor: '#141414',
						borderRadius: 4,
						borderSkipped: false,
						data: [],
						maxBarThickness: 10,
					},
					{
						type: "line",
						label: '',
						tension: 0.4,
						borderWidth: 0,
						pointRadius: 0,
						pointBackgroundColor: "#1890FF",
						borderColor: "#1890FF",
						borderWidth: 2,
						backgroundColor: null,
						data: [],
						fill: true,
					}
				],
			},
			barChartData: {
				labels: [],
				datasets: [
					{
						label: "",
						backgroundColor: '#fff',
						borderWidth: 0,
						borderSkipped: false,
						borderRadius: 6,
						data: [],
						maxBarThickness: 20,
					}
				],
			},
			storageMonitoringData: {},
			latestData: [],
			previousData: [],
			queryParams: {
				limit: 10,
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
			if (this.storageId) {
				this.queryParams.storageId = this.storageId
				this.queryParams.dataType = 3
			}
			this.getStorageMonitorings()
			this.getStorageMonitoringPreviousData()
			this.getStorageMonitoringData()
			this.getStorageDeviceCount()
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
		getStorageDeviceCount() {
			this.storageDeviceCount = '-'
			getStorageMonitoringList({ isLatest: true, dataType: 5 }).then(response => {
				if (response && response.length > 0) {
					this.storageDeviceCount = response.length
				}
			})
		},
		getStorageMonitorings() {
			this.getStorageMonitoringPreviousData().then(() => {
				this.storageMonitoringData = {
					storageId : this.queryParams.storageId
				}
				getStorageMonitoringList({ isLatest: true, dataType: this.queryParams.dataType, storageId: this.queryParams.storageId }).then(response => {
					this.latestData = []
					if (response && response.length > 0) {
						let data = response[0]
						this.storageMonitoringData = data
						let filesSizeDiff = data.filesSize - this.previousData.filesSize
						let filesCountDiff = data.filesCount - this.previousData.filesCount
						let artifactsSizeDiff = data.artifactsSize - this.previousData.artifactsSize
						let artifactsCountDiff = data.artifactsCount - this.previousData.artifactsCount
						let foldersCountDiff = data.foldersCount - this.previousData.foldersCount
						let repositoryCountDiff = data.repositoryCount - this.previousData.repositoryCount
						let storageCountDiff = data.storageCount - this.previousData.storageCount
						this.storageMonitoringData.filesSizeDiff = filesSizeDiff
						this.storageMonitoringData.filesCountDiff = filesCountDiff
						this.storageMonitoringData.artifactsSizeDiff = artifactsSizeDiff
						this.storageMonitoringData.artifactsCountDiff = artifactsCountDiff
						this.storageMonitoringData.foldersCountDiff = foldersCountDiff
						this.storageMonitoringData.repositoryCountDiff = repositoryCountDiff
						this.storageMonitoringData.storageCountDiff = storageCountDiff
						this.latestData.push({
							id: "1",
							title: 'StorageMonitoring.FilesSize',
							code: this.fileSizeConver(data.filesSize),
							avatar: "file",
							color: '#1890FF',
							previousDiff: filesSizeDiff,
							artifactsDownloadedCount: data.artifactsDownloadedCount
						})
						this.latestData.push({
							id: "2",
							title: 'StorageMonitoring.FilesCount',
							code: data.filesCount,
							avatar: "file-text",
							color: '#1890FF',
							previousDiff: filesCountDiff,
							artifactsDownloadedCount: data.artifactsDownloadedCount
						})
						this.latestData.push({
							id: "3",
							title: 'StorageMonitoring.ArtifactsSize',
							code: this.fileSizeConver(data.artifactsSize),
							avatar: "appstore",
							color: '#1890FF',
							previousDiff: artifactsSizeDiff,
							artifactsDownloadedCount: data.artifactsDownloadedCount
						})
						this.latestData.push({
							id: "4",
							title: 'StorageMonitoring.ArtifactsCount',
							code: data.artifactsCount,
							src: "images/folib/artifactCount.svg",
							color: '',
							previousDiff: artifactsCountDiff,
							artifactsDownloadedCount: data.artifactsDownloadedCount
						})
						this.latestData.push({
							id: "5",
							title: 'StorageMonitoring.FoldersCount',
							code: data.foldersCount,
							avatar: "folder",
							color: '#1890FF',
							previousDiff: foldersCountDiff,
							artifactsDownloadedCount: data.artifactsDownloadedCount
						})
						if (!this.storageId) {
							this.latestData.push({
								id: "6",
								title: 'StorageMonitoring.StorageCount',
								code: data.storageCount,
								src: "images/folib/storageCount.svg",
								color: '',
								previousDiff: storageCountDiff,
								artifactsDownloadedCount: data.artifactsDownloadedCount
							})
						} else {
							this.latestData.push({
								id: "6",
								title: 'StorageMonitoring.StorageCount',
								code: 1,
								src: "images/folib/storageCount.svg",
								color: '',
								previousDiff: 0,
								artifactsDownloadedCount: data.artifactsDownloadedCount
							})
						}
						this.latestData.push({
							id: "7",
							title: 'StorageMonitoring.RepositoryCount',
							code: data.repositoryCount,
							src: "images/folib/repositoryCount.svg",
							color: '',
							previousDiff: repositoryCountDiff,
							artifactsDownloadedCount: data.artifactsDownloadedCount
						})
						this.latestData.push({
							id: "8",
							title: 'StorageMonitoring.DataUpdateTime',
							code: data.createTime,
							avatar: "clock-circle",
							color: '#1890FF',
							previousDiff: 0,
							artifactsDownloadedCount: data.artifactsDownloadedCount
						})
					}
				})
			})
		},
		getStorageMonitoringPreviousData() {
			return new Promise((resolve, reject) => {
				this.previousData = [];
				getStorageMonitoringPage({ dataType: this.queryParams.dataType, storageId: this.queryParams.storageId, sortField: 'createTime', sortOrder: 'DESC' }).then((res) => {
					let data = res.data.rows;
					if (data && data.length > 1) {
						this.previousData = data[1];
					}
					resolve();
				}).catch(reject);
			});
		},
		getStorageMonitoringData() {
			getStorageMonitoringPage(this.queryParams).then((res) => {
				let data = res.data.rows
				if (data) {
					this.storageData = [...data].reverse()
				} else {
					this.storageData = []
				}
				this.barChartData.labels = []
				this.barChartData.datasets[0].data = []

				this.mixedChartData.labels = []
				this.mixedChartData.datasets[0].data = []
				this.mixedChartData.datasets[1].data = []

				this.lineChartData.labels = []
				this.lineChartData.datasets[0].data = []
				this.lineChartData.datasets[1].data = []
				for (let index in this.storageData) {
					let storage = this.storageData[index]
					this.barChartData.labels.push(storage.createDay)
					this.barChartData.datasets[0].data.push(this.fileSizeConverUnit(storage.filesSize))

					this.mixedChartData.labels.push(storage.createDay)
					this.mixedChartData.datasets[0].data.push(this.fileSizeConverUnit(storage.filesSize))
					this.mixedChartData.datasets[1].data.push(this.fileSizeConverUnit(storage.artifactsSize))

					this.lineChartData.labels.push(storage.createDay)
					this.lineChartData.datasets[0].data.push(storage.filesCount)
					this.lineChartData.datasets[1].data.push(storage.artifactsCount)
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