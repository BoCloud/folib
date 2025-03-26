<template>
  <div class="anonymous-home">
     <!-- Header Background Image -->
     <div class="profile-nav-bg" style="
            background: #fff;
            transition: all 0.3s;
          ">
      <a-row type="flex" :md="8" :xs="4">
        <SearchBox @mouse="searchBoxMouseStatus" @search="search"/>
      </a-row>
    </div>
    <Storages v-if="!searchVisible && (allowAnonymous || isLogin())" :anonymous="true" class="mt-15"/>
    <Search ref="search" class="mt-20" v-if="searchVisible && (allowAnonymous || isLogin())" :columns="columns"/>
  </div>
</template>
<script>
import {
  getAllowAnonymous,
} from '@/api/settings'
import Storages from "./Storages.vue"
import SearchBox from "@/components/Tools/SearchBox"
import Search from "./components/Search/index.vue"
import { isLogin } from "@/utils/permission"

export default {
  data() {
    return {
      mouseEnter: false,
      columns: [
        {
          i18nKey: 'Store.OwnedStorage',
          title: this.$t('Store.OwnedStorage'),
          dataIndex: "storageId",
          scopedSlots: { customRender: "storageId" },
          width: 150,
        },
        {
          i18nKey: 'Store.OwnedWarehouse',
          title: this.$t('Store.OwnedWarehouse'),
          dataIndex: 'repositoryId',
          scopedSlots: { customRender: 'repositoryId' },
          width: 150
        },
        {
          i18nKey: 'Store.ProductPath',
          title: this.$t('Store.ProductPath'),
          dataIndex: 'path',
          scopedSlots: { customRender: 'path' },
          width: 550
        },
        {
          i18nKey: 'Store.CreationTime',
          title: this.$t('Store.CreationTime'),
          dataIndex: 'created',
          sorter: true,
          sortDirections: ['descend', 'ascend'],
          scopedSlots: { customRender: 'created' },
          width: 200
        },
        {
          i18nKey: 'Store.LastUsedTime',
          title: this.$t('Store.LastUsedTime'),
          dataIndex: 'lastUsed',
          sorter: true,
          scopedSlots: { customRender: 'lastUsed' },
          width: 200
        },
        {
          i18nKey: 'Store.DownloadTimes',
          title: this.$t('Store.DownloadTimes'),
          dataIndex: 'downloadCount',
          sorter: true,
          scopedSlots: { customRender: 'created' },
          width: 200
        },
        {
          i18nKey: 'Store.ProductSize',
          title: this.$t('Store.ProductSize'),
          dataIndex: 'sizeInBytes',
          sorter: true,
          scopedSlots: { customRender: 'sizeInBytes' },
          width: 200
        }
      ],
      searchVisible: false,
      allowAnonymous: false,
    }
  },
  components: {
    Storages,
    SearchBox,
    Search,
  },
  created() {
    this.getAllowAnonymous()
  },
	watch: {
    
  },
  mounted() {},
  methods: {
    searchBoxMouseStatus(bool) {
      this.mouseEnter = bool
    },
    search(value, searchType, type) {
      this.searchVisible = true
      this.$nextTick(() => {
        this.$refs.search.search(value, searchType, type)
      })
    },
    isLogin() {
      return isLogin()
    },
    getAllowAnonymous() {
      getAllowAnonymous().then(res => {
        this.allowAnonymous = res
        let login = isLogin()
        console.log("isLogin: " + login)
        if(!login &&!this.allowAnonymous) {
          console.log("allowAnonymous: " + this.allowAnonymous + " to /login")
          this.$router.push('/login')
        }
      })
    },
  }
}
</script>

<style lang="scss" scoped>
.anonymous-home::v-deep {
  .profile-nav-bg {
    display: flex;
    justify-content: center;
    align-items: center;
    color: #fafafa;
    //position: relative;
    overflow: hidden;
    width: 100%;
  }

  .nested {
    position: absolute;
    left: 0;
    right: 0;
    top: 0;
    bottom: 0;
  }

}
</style>
