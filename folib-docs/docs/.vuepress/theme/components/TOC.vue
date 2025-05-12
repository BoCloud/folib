<template>
  <nav class="toc">
    <div class="content-text">文章内容</div>
    <el-timeline class="content_list" :key="key">
      <el-timeline-item v-for="(item, index) in tocItems" :key="index" :class="{ isActive: activeHeading == item.id }">
        <a class="content_header" @click="scrollToSection(item.id)" :href="`#${item.id}`">{{ item.text }}</a>
      </el-timeline-item>
    </el-timeline>
  </nav>
</template>

<script>
export default {
  name: 'TOC',
  props: {
    tocItems: {
      type: Array,
      required: true,
    },
  },
  data() {
    return {
      activeHeading: '',
      key: 0
    };
  },
  watch:{
    tocItems(val, oldVal){
        // if (JSON.stringify(val) !== JSON.stringify(oldVal)) 
        this.activeHeading = val[0].id
    },
    $route(){
      this.key++
    }
  },
  mounted() {
    this.updateActiveHeading(100, true)
  },
  methods: {
    updateActiveHeading(scrollPosition,isAtBottom) {
      const headings = Array.from(document.querySelectorAll('.content__default h2')) // 获取所有内容标题
      for (const heading of headings.reverse()) {
        if (heading.offsetTop <= scrollPosition + 100) { // 判断滚动位置
          this.activeHeading = heading.id; // 更新当前高亮标题 ID
          break
        }
      }
      // 防止内容不够 最后一个样式无法渲染
      if(isAtBottom){
        this.activeHeading = headings.reverse().at(0).id
      }
    },
    scrollToSection(id) {
      this.activeHeading = id
      const target = document.getElementById(id)
      target.style.scrollMarginTop = '60px'
    },
  }
};
</script>

<style lang="scss">
.toc {
  width: 230px;
  position: fixed;
  z-index: 0;
  right: 18px;
  top: 98px;
  padding-right: 0px;
  // height: calc(100% - 100px);
  overflow: auto;
  background-color: #fff;
  box-sizing: border-box;
  // background-image: url('../../components/images/robot.png');
  background-repeat: no-repeat;
  background-position: calc(100% + 44px) 50%;
}

.content-text {
  padding-left: 36px;
  font-size: 14px;
  font-weight: 600;
  color: #393b3e;
}

.content_list {
  margin-top: 20px;
  margin-left: 20px;
  font-size: 14px;
  height: calc(100vh - 165px);
  overflow-y: auto;

  .el-timeline-item{
    padding-bottom: 10px;
  }

  .el-timeline-item__tail {
    border-left: 2px dotted #D8D8D8;
    top: 5px;
  }

  .el-timeline-item__node--normal {
    transition: all 0.2s;
    top: 5px;
    left: 2px;
    width: 6px;
    height: 6px;
  }

  .isActive{
    .content_header{
      transition: all 0.2s;
      color: #0A6AFF;
      font-weight: 600;
    }
    
    .el-timeline-item__node--normal {
      background: #0A6AFF;
      left: 1px;
      width: 8px;
      height: 8px;
    }
  }
}

.content_header {
  color: #666;
}
</style>