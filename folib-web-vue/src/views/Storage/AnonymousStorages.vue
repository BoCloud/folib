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
        <SearchBox @mouse="searchBoxMouseStatus"/>
      </a-row>
    </div>
    <Storages :anonymous="true" class="mt-15"/>
  </div>
</template>
<script>
import Storages from "../Storage/Storages.vue"
import SearchBox from "@/components/Tools/SearchBox"

export default {
  data() {
    return {
      mouseEnter: false,
    }
  },
  components: {
    Storages,
    SearchBox,
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