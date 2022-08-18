<template>

	<!-- Invoices Card -->
	<a-card :bordered="false" class="header-solid h-full" :bodyStyle="{paddingTop: 0, paddingBottom: '16px' }">
		<template #title>
			<h6 class="font-semibold m-0">角色选择</h6>
		</template>
		<template slot="extra" class="mb-0">
			<a-button type="primary">
				全选
			</a-button>
		</template>
    <hr class="gradient-line">
    <a-row type="flex" align="middle">
      <a-col>
        <a-avatar :size="48" src="images/folib/admin.svg" />
      </a-col>
      <a-col class="pl-15">
        <h5 class="mb-0">Asana</h5>
        <a class="text-dark">Organize your team</a>
      </a-col>
      <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
        <span class="mr-15">{{ asana ? 'Enabled' : 'Disabled' }}</span>
        <a-switch default-checked v-model="asana" />
      </a-col>
    </a-row>

  

    <hr class="gradient-line">
    <a-row type="flex" align="middle">
      <a-col>
        <a-avatar :size="48" src="images/folib/artifact_manager.svg" />
      </a-col>
      <a-col class="pl-15">
        <h5 class="mb-0">Asana</h5>
        <a class="text-dark">Organize your team</a>
      </a-col>
      <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
        <span class="mr-15">{{ asana ? 'Enabled' : 'Disabled' }}</span>
        <a-switch default-checked v-model="asana" />
      </a-col>
    </a-row>
    <hr class="gradient-line">
    <a-row type="flex" align="middle">
      <a-col>
        <a-avatar :size="48" src="images/small-logos/logo-asana.svg" />
      </a-col>
      <a-col class="pl-15">
        <h5 class="mb-0">Asana</h5>
        <a class="text-dark">Organize your team</a>
      </a-col>
      <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
        <span class="mr-15">{{ asana ? 'Enabled' : 'Disabled' }}</span>
        <a-switch default-checked v-model="asana" />
      </a-col>
    </a-row>
    <hr class="gradient-line">
    <a-row type="flex" align="middle">
      <a-col>
        <a-avatar :size="48" src="images/small-logos/logo-asana.svg" />
      </a-col>
      <a-col class="pl-15">
        <h5 class="mb-0">Asana</h5>
        <a class="text-dark">Organize your team</a>
      </a-col>
      <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
        <span class="mr-15">{{ asana ? 'Enabled' : 'Disabled' }}</span>
        <a-switch default-checked v-model="asana" />
      </a-col>
    </a-row>
    <hr class="gradient-line">
    <a-row type="flex" align="middle">
      <a-col>
        <a-avatar :size="48" src="images/small-logos/logo-asana.svg" />
      </a-col>
      <a-col class="pl-15">
        <h5 class="mb-0">Asana</h5>
        <a class="text-dark">Organize your team</a>
      </a-col>
      <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
        <span class="mr-15">{{ asana ? 'Enabled' : 'Disabled' }}</span>
        <a-switch default-checked v-model="asana" />
      </a-col>
    </a-row>
    <hr class="gradient-line">
    <a-row type="flex" align="middle">
      <a-col>
        <a-avatar :size="48" src="images/small-logos/logo-asana.svg" />
      </a-col>
      <a-col class="pl-15">
        <h5 class="mb-0">Asana</h5>
        <a class="text-dark">Organize your team</a>
      </a-col>
      <a-col :span="24" :md="12" class="ml-auto" style="display: flex; align-items: center; justify-content: flex-end">
        <span class="mr-15">{{ asana ? 'Enabled' : 'Disabled' }}</span>
        <a-switch default-checked v-model="asana" />
      </a-col>
    </a-row>
	</a-card>
	<!-- / Invoices Card -->

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