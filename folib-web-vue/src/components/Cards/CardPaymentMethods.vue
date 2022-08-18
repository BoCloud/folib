<template>

	<!-- Payment Methods Card -->
	<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0,}">
		<template #title>
			<a-row type="flex" align="middle">
				<a-col :span="24" :md="12">
					<h6 class="font-semibold m-0">用户管理</h6>
				</a-col>
				<a-col :span="24" :md="12" style="display: flex; align-items: center; justify-content: flex-end">
					<a-button type="primary">
						添加 用户
					</a-button>
				</a-col>
			</a-row>
		</template>
		<a-row :gutter="[24, 24]">
			<a-col :span="24" :md="12">
				<a-card class="payment-method-card">
					<img src="images/folib/userAdmin.svg" alt="">
					<h6 class="card-number">用户总数</h6>
					<a-button type="link">
            <h6 class="card-number">20</h6>
					</a-button>
				</a-card>
			</a-col>
			<a-col :span="24" :md="12">
				<a-card class="payment-method-card">
					<img src="images/folib/userCount.svg" alt="">
					<h6 class="card-number">管理员数量</h6>
          <a-button type="link">
            <h6 class="card-number">1</h6>
          </a-button>
				</a-card>
			</a-col>
		</a-row>
	</a-card>
	<!-- Payment Methods Card -->

</template>

<script>

	export default ({
		data() {
			return {
			}
		},
	})

</script>