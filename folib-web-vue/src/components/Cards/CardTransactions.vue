<template>

	<!-- Your Transactions Card -->
	<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }">
		<template #title>
			<h6 class="font-semibold m-0">Your Transactions</h6>
		</template>
		<p slot="extra" class="card-header-date">
			<a-icon type="calendar" theme="filled" />
			<span>23 - 30 March 2021</span>
		</p>
		<a-list
			class="transactions-list"
			item-layout="horizontal"
			:split="false"
			:data-source="data"
		>
			<a-list-item slot="renderItem" slot-scope="item">
				<template v-if="item.period">
					<h6>{{ item.period }}</h6>
				</template>
				<template v-else>
					<a-list-item-meta
						:title="item.title"
						:description="item.datetime"
					>
						<a-avatar size="small" v-if="item.type == 1" slot="avatar" style="background-color: #EDF9E7">
							<svg width="10" height="10" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
								<path class="fill-success" fill-rule="evenodd" clip-rule="evenodd" d="M10 3C10.5523 3 11 3.44772 11 4V9H16C16.5523 9 17 9.44772 17 10C17 10.5523 16.5523 11 16 11H11V16C11 16.5523 10.5523 17 10 17C9.44772 17 9 16.5523 9 16V11H4C3.44772 11 3 10.5523 3 10C3 9.44771 3.44772 9 4 9L9 9V4C9 3.44772 9.44772 3 10 3Z"/>
							</svg>
						</a-avatar>
						<a-avatar size="small" v-if="item.type == 0" slot="avatar" style="background-color: #FFFCE7">
							<strong class="text-warning">
								!
							</strong>
						</a-avatar>
						<a-avatar size="small" v-if="item.type == -1" slot="avatar" style="background-color: #FEE9EA">
							<svg width="10" height="10" viewBox="0 0 20 20" fill="none" xmlns="http://www.w3.org/2000/svg">
								<path class="fill-danger" fill-rule="evenodd" clip-rule="evenodd" d="M5 10C5 9.44772 5.44772 9 6 9L14 9C14.5523 9 15 9.44772 15 10C15 10.5523 14.5523 11 14 11L6 11C5.44772 11 5 10.5523 5 10Z"/>
							</svg>
						</a-avatar>
					</a-list-item-meta>
					<div class="amount">
						<span v-if="item.type == 1" class="text-success">
							+ ${{ item.amount }}
						</span>
						<span v-if="item.type == 0" class="text-warning">
							Pending
						</span>
						<span v-if="item.type == -1" class="text-danger">
							- ${{ item.amount }}
						</span>
					</div>
				</template>
			</a-list-item>
		</a-list>
	</a-card>
	<!-- / Your Transactions Card -->

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