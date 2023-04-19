<template>
  <div class="anonymous-storages">
     <!-- Header Background Image -->
     <div class="profile-nav-bg">
        <div
          :class="[mouseEnter ? 'mouse-enter nested' : 'nested']"
          style="
            background: url(images/bg-profile.jpg) center/cover;
            transition: all 0.3s;
          "
        ></div>
      <a-row type="flex" :md="8" :xs="4">
        <SearchBox @mouse="searchBoxMouseStatus" @search="search"/>
      </a-row>
    </div>
    <Storages v-if="!searchVisible" :anonymous="true" class="mt-15"/>
    <Search ref="search" class="mt-20" v-if="searchVisible" :openRepository="true" :columns="columns"/>
  </div>
</template>
<script>
import Storages from "../Storage/Storages.vue"
import SearchBox from "@/components/Tools/SearchBox"
import Search from "../Storage/components/Search/index.vue"

export default {
  data() {
    return {
      mouseEnter: false,
      columns: [
        {
          title: "存储空间",
          dataIndex: "storageId",
          scopedSlots: { customRender: "storageId" },
          width: 150,
        },
        {
          title: "所属仓库",
          dataIndex: "repositoryId",
          scopedSlots: { customRender: "repositoryId" },
          width: 150,
        },
        {
          title: "制品路径",
          dataIndex: "path",
          scopedSlots: { customRender: "path" },
          width: 550,
        },
        {
          title: "创建时间",
          dataIndex: "created",
          sorter: true,
          sortDirections: ["descend", "ascend"],
          scopedSlots: { customRender: "created" },
          width: 200,
        },
        {
          title: "最近使用时间",
          dataIndex: "lastUsed",
          sorter: true,
          scopedSlots: { customRender: "lastUsed" },
          width: 200,
        },
        {
          title: "下载次数",
          dataIndex: "downloadCount",
          sorter: true,
          scopedSlots: { customRender: "created" },
          width: 200,
        },
        {
          title: "制品大小",
          dataIndex: "sizeInBytes",
          sorter: true,
          scopedSlots: { customRender: "sizeInBytes" },
          width: 200,
        },
      ],
      searchVisible: false,
    }
  },
  components: {
    Storages,
    SearchBox,
    Search,
  },
  created() {
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
  }
}
</script>

<style lang="scss" scoped>
.anonymous-storages::v-deep {
  .profile-nav-bg {
    display: flex;
    justify-content: center;
    align-items: center;
    color: #fafafa;
    position: relative;
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