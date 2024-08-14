
<template>
	<div class="storage-monitoring">
    <Overview/>
		<!-- <StorageDeviceInfo class="mt-20"/> -->
    <StorageDeviceInfo2 class="mt-20"/>
    <StorageInfo class="mt-20"/>
	</div>
</template>

<script>
import Overview from "./components/Overview"
import StorageDeviceInfo from "./components/StorageDeviceInfo"
import StorageDeviceInfo2 from "./components/StorageDeviceInfo2"
import StorageInfo from "./components/StorageInfo"


export default ({
	components: {
    Overview,
		StorageDeviceInfo,
    StorageDeviceInfo2,
    StorageInfo,
	},
  created() {
		this.init()
  },
  data() {
    return {}
  },
  methods: {
		message(type, message) {
      if (!message) {
        message = "操作成功"
      }
      this.$notification[type]({
        message: message,
        description: "",
      })
    },
		init() {
			
		}
  }
})

</script>

<style lang="scss">
.storage-monitoring {
  .item{
    width: 100%;
    height: 10px;
  }
  .progress1 {
    background-color: red;
    border-radius: 5px 0 0 5px;
  }
  .progress2 {
    background-color: green;
  }
  .progress3 {
    background-color: black;
    border-radius: 0 5px 5px 0;
  }
}
</style>

