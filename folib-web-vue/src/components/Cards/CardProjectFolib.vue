<template>

	<!-- Project 2 Card -->
	<a-card :bordered="false" class="card-project-2 header-solid" :bodyStyle="{paddingTop: '14px' }" :headStyle="{paddingBottom: '0' }">
		<template #title>
			<a-row type="flex" :gutter="[24,24]" align="middle">
				<a-col>
					<a-avatar
						:size="48"
						shape="square"
						style="border-radius: 8px; background-image: linear-gradient( 310deg, #020202, #5c6391 );"
					>
						<img :src="logo" style="width: 100%;" alt="">
					</a-avatar>
				</a-col>
				<a-col @click="goToDetial">
          <a >
					<h6 class="font-semibold mb-10">
            <textOver
                :text="title"
                :max="14"
            />
          </h6>
          </a>
					<a-space :size="-12" class="avatar-chips">
						<a-avatar size="small" v-for="(src, key) in team" :key="key" :src="src" />
					</a-space>
				</a-col>
				<a-col class="ml-auto pt-10" style="align-self: flex-start;">
          <a-dropdown v-if="operatorEnabled">
          <a>
            <a-icon type="more" class="text-muted" style="font-size: 18px;" />
          </a>
            <template #overlay>
              <a-menu slot="overlay" @click="handleMenuClick">
                <a-menu-item key="edit" v-if="editEnabled"> <a-icon type="edit" />{{ $t('Cards.Edit') }}</a-menu-item>
                <a-menu-item key="delete" v-if="deleteEnabled"> <a-icon type="delete" />{{ $t('Cards.Delete') }}</a-menu-item>
              </a-menu>
            </template>
          </a-dropdown>
				</a-col>
			</a-row>
		</template>
		<p><slot></slot></p>
		<hr class="gradient-line">
		<a-row type="flex" :gutter="[24,24]" align="middle">
			<a-col>

				<h6 class="font-semibold text-md mb-0">{{ due }}</h6>
				<p class="font-semibold text-muted mb-0">{{ $t('Cards.WarehouseType') }}</p>
			</a-col>
			<a-col class="ml-auto">
				<a-icon v-if="repository.scope===2" :style="{fontSize: '18px', color:'#52C41A'}" type="unlock" />
<!--        <a-tag v-if="due==='snapshot'" class="text-md bg-warning">{{ due }}</a-tag>-->
<!--        <a-tag v-if="due==='release'" class="text-md bg-secondary text-white">{{ due }}</a-tag>-->
<!--        <a-tag v-if="due==='mixed'" class="text-md bg-primary text-white">{{ due }}</a-tag>-->
<!--				<p class="font-semibold text-muted mb-0">版本类型</p>-->
			</a-col>
		</a-row>
	</a-card>
	<!-- / Project 2 Card -->

</template>

<script>
import { hasRole, isAdmin, hasPermission } from "@/utils/permission";
import textOver from "@/components/Tools/textOver";
export default ({
  components: {
    textOver
  },
	props: {
		title: {
			type: String,
			default: "",
		},
		logo: {
			type: String,
			default: "",
		},
		team: {
			type: Array,
			default: () => [],
		},
		participants: {
			type: [Number, String],
			default: 0,
		},
		due: {
			type: String,
			default: "",
		},
		repository: {
			type: Object,
			default: () => {},
		},
		storageAdmin: {
			type: String,
			default: "",
		},
	},
	data() {
		return {
			operatorEnabled: false,
			editEnabled: false,
			deleteEnabled: false,
		}
	},
	created() {
		this.init()
	},
	watch: {
		storageAdmin: function (newval, oldVal) {
			this.init()
		},
  	},
    methods:{
      handleMenuClick(e){
        this.$emit("handleMenuClick",e.key,this.title)
      },
      goToDetial(e){
        this.$emit("goToDetial",e.key,this.title)
      },
	  init() {
		this.editEnabled = (isAdmin() || this.storageAdmin === this.$store.state.user.name)
		this.deleteEnabled = (isAdmin() || this.storageAdmin === this.$store.state.user.name) && (this.repository.allowsDeletion || this.repository.allowsForceDeletion)
		console.log(this.repository.storageId, this.repository.id, this.storageAdmin ,  this.$store.state.user.name, this.editEnabled, this.deleteEnabled)

		this.operatorEnabled = this.editEnabled || this.deleteEnabled
	  },
    }
})
</script>
