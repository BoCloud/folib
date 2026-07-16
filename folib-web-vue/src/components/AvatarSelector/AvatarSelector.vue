<template>
  <div>
    <a-tabs>
      <a-tab-pane v-for="(v, i) in icons" :tab="v.title" :key="i">
        <ul>
          <li v-for="(icon, key) in v.icons" :key="`${v.title}-${key}`" :class="{ 'active': selectedIcon==icon }">
            <svg class="icon" :style="{ fontSize: '32px' }" aria-hidden="true" @click="handleSelectedIcon(icon)">
              <use :xlink:href="'#'+icon"></use>
            </svg>
          </li>
        </ul>
      </a-tab-pane>
    </a-tabs>
  </div>
</template>

<script>
import icons from './icons'

export default {
  name: 'AvatarSelect',
  data () {
    return {
      selectedIcon: '',
      icons
    }
  },
  methods: {
    handleSelectedIcon (icon) {
      this.selectedIcon = icon
      this.$emit('avatarChange', icon)
    }
  }
}
</script>

<style lang="scss" scoped>
  ul{
    list-style: none;
    padding: 0;
    overflow-y: scroll;
    height: 350px;
    li{
      display: inline-block;
      padding:5px;
      margin:5px;
      &:hover {
        cursor: pointer;
      }
      &.active{
        box-shadow: 0px 0px 5px 2px #888888;
      }
    }
  }
</style>
