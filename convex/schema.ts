import { defineTable, defineSchema } from "convex/server";
import { v } from "convex/values";

export default defineSchema({
  todos: defineTable({
    title: v.string(),
    description: v.string(),
    isCompleted: v.boolean(),
  }),
});
